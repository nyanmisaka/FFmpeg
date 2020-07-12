/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * scale video filter - AMF
 */

#include <stdio.h>
#include <string.h>

#include "libavutil/avassert.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/time.h"

#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_amf.h"
#include "libavcodec/amf.h"

#include "AMF/components/VideoConverter.h"

#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "video.h"
#include "scale_eval.h"

#if CONFIG_DXVA2
#include <d3d9.h>
#endif

#if CONFIG_D3D11VA
#include <d3d11.h>
#endif

typedef struct AMFScaleContext {
    const AVClass *class;

    int width, height;
    enum AVPixelFormat format;
    int scale_type;

    char *w_expr;
    char *h_expr;
    char *format_str;

    AMFComponent        *converter;
    AVBufferRef         *amf_device_ref;

    AVBufferRef         *hwframes_in_ref;
    AVBufferRef         *hwframes_out_ref;
    AVBufferRef         *hwdevice_ref;

    AMFContext          *context;
    AMFFactory          *factory;

} AMFScaleContext;


static int amf_copy_surface(AVFilterContext *avctx, const AVFrame *frame,
    AMFSurface* surface)
{
    AMFPlane *plane;
    uint8_t  *dst_data[4];
    int       dst_linesize[4];
    int       planes;
    int       i;

    planes = surface->pVtbl->GetPlanesCount(surface);
    av_assert0(planes < FF_ARRAY_ELEMS(dst_data));

    for (i = 0; i < planes; i++) {
        plane = surface->pVtbl->GetPlaneAt(surface, i);
        dst_data[i] = plane->pVtbl->GetNative(plane);
        dst_linesize[i] = plane->pVtbl->GetHPitch(plane);
    }
    av_image_copy(dst_data, dst_linesize,
        (const uint8_t**)frame->data, frame->linesize, frame->format,
        frame->width, frame->height);

    return 0;
}

static void amf_free_amfsurface(void *opaque, uint8_t *data)
{
    AMFSurface *surface = (AMFSurface*)(opaque);
    surface->pVtbl->Release(surface);
}

static AVFrame *amf_amfsurface_to_avframe(AVFilterContext *avctx, AMFSurface* pSurface)
{
    AVFrame *frame = av_frame_alloc();

    if (!frame)
        return NULL;

    switch (pSurface->pVtbl->GetMemoryType(pSurface))
    {
#if CONFIG_D3D11VA
        case AMF_MEMORY_DX11:
        {
            AMFPlane *plane0 = pSurface->pVtbl->GetPlaneAt(pSurface, 0);
            frame->data[0] = plane0->pVtbl->GetNative(plane0);
            frame->data[1] = (uint8_t*)(intptr_t)0;

            frame->buf[0] = av_buffer_create(NULL,
                                     0,
                                     amf_free_amfsurface,
                                     pSurface,
                                     AV_BUFFER_FLAG_READONLY);
            pSurface->pVtbl->Acquire(pSurface);
        }
        break;
#endif
#if CONFIG_DXVA2
        case AMF_MEMORY_DX9:
        {
            AMFPlane *plane0 = pSurface->pVtbl->GetPlaneAt(pSurface, 0);
            frame->data[3] = plane0->pVtbl->GetNative(plane0);

            frame->buf[0] = av_buffer_create(NULL,
                                     0,
                                     amf_free_amfsurface,
                                     pSurface,
                                     AV_BUFFER_FLAG_READONLY);
            pSurface->pVtbl->Acquire(pSurface);
        }
        break;
#endif
    default:
        {
            av_assert0(0);//should not happen
        }
    }

    return frame;
}

static int amf_avframe_to_amfsurface(AVFilterContext *avctx, const AVFrame *frame, AMFSurface** ppSurface)
{
    AMFScaleContext *ctx = avctx->priv;
    AMFSurface *surface;
    AMF_RESULT  res;
    int hw_surface = 0;

    switch (frame->format) {
#if CONFIG_D3D11VA
    case AV_PIX_FMT_D3D11:
        {
            static const GUID AMFTextureArrayIndexGUID = { 0x28115527, 0xe7c3, 0x4b66, { 0x99, 0xd3, 0x4f, 0x2a, 0xe6, 0xb4, 0x7f, 0xaf } };
            ID3D11Texture2D *texture = (ID3D11Texture2D*)frame->data[0]; // actual texture
            int index = (intptr_t)frame->data[1]; // index is a slice in texture array is - set to tell AMF which slice to use
            texture->lpVtbl->SetPrivateData(texture, &AMFTextureArrayIndexGUID, sizeof(index), &index);

            res = ctx->context->pVtbl->CreateSurfaceFromDX11Native(ctx->context, texture, &surface, NULL); // wrap to AMF surface
            AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR(ENOMEM), "CreateSurfaceFromDX11Native() failed  with error %d\n", res);
            hw_surface = 1;
        }
        break;
#endif
#if CONFIG_DXVA2
    case AV_PIX_FMT_DXVA2_VLD:
        {
            IDirect3DSurface9 *texture = (IDirect3DSurface9 *)frame->data[3]; // actual texture

            res = ctx->context->pVtbl->CreateSurfaceFromDX9Native(ctx->context, texture, &surface, NULL); // wrap to AMF surface
            AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR(ENOMEM), "CreateSurfaceFromDX9Native() failed  with error %d\n", res);
            hw_surface = 1;
        }
        break;
#endif
    default:
        {
            AMF_SURFACE_FORMAT amf_fmt = amf_av_to_amf_format(frame->format);
            res = ctx->context->pVtbl->AllocSurface(ctx->context, AMF_MEMORY_HOST, amf_fmt, frame->width, frame->height, &surface);
            AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR(ENOMEM), "AllocSurface() failed  with error %d\n", res);
            amf_copy_surface(avctx, frame, surface);
        }
        break;
    }

    if (hw_surface) {
        // input HW surfaces can be vertically aligned by 16; tell AMF the real size
        surface->pVtbl->SetCrop(surface, 0, 0, frame->width, frame->height);
    }

    surface->pVtbl->SetPts(surface, frame->pts);
    *ppSurface = surface;
    return 0;
}

static int amf_scale_init(AVFilterContext *avctx)
{
    AMFScaleContext     *ctx = avctx->priv;

    if (!strcmp(ctx->format_str, "same")) {
        ctx->format = AV_PIX_FMT_NONE;
    } else {
        ctx->format = av_get_pix_fmt(ctx->format_str);
        if (ctx->format == AV_PIX_FMT_NONE) {
            av_log(avctx, AV_LOG_ERROR, "Unrecognized pixel format: %s\n", ctx->format_str);
            return AVERROR(EINVAL);
        }
    }

    return 0;
}

static void amf_scale_uninit(AVFilterContext *avctx)
{
    AMFScaleContext *ctx = avctx->priv;

    if (ctx->converter) {
        ctx->converter->pVtbl->Terminate(ctx->converter);
        ctx->converter->pVtbl->Release(ctx->converter);
        ctx->converter = NULL;
    }

    av_buffer_unref(&ctx->amf_device_ref);
    av_buffer_unref(&ctx->hwdevice_ref);
    av_buffer_unref(&ctx->hwframes_in_ref);
    av_buffer_unref(&ctx->hwframes_out_ref);
}

static int amf_scale_query_formats(AVFilterContext *avctx)
{
    const enum AVPixelFormat *output_pix_fmts;
    AVFilterFormats *input_formats;
    int err;
    int i;
    static const enum AVPixelFormat input_pix_fmts[] = {
        AV_PIX_FMT_NV12,
        AV_PIX_FMT_0RGB,
        AV_PIX_FMT_BGR0,
        AV_PIX_FMT_RGB0,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_YUV420P,
        AV_PIX_FMT_YUYV422,
        AV_PIX_FMT_NONE,
    };
    static const enum AVPixelFormat output_pix_fmts_default[] = {
        AV_PIX_FMT_D3D11,
        AV_PIX_FMT_DXVA2_VLD,
        AV_PIX_FMT_NONE,
    };
    output_pix_fmts = output_pix_fmts_default;

    //in case if hw_device_ctx is set to DXVA2 we change order of pixel formats to set DXVA2 be choosen by default
    //The order is ignored if hw_frames_ctx is not NULL on the config_output stage
    if (avctx->hw_device_ctx) {
        AVHWDeviceContext *device_ctx = (AVHWDeviceContext*)avctx->hw_device_ctx->data;

        switch (device_ctx->type) {
    #if CONFIG_D3D11VA
        case AV_HWDEVICE_TYPE_D3D11VA:
            {
                static const enum AVPixelFormat output_pix_fmts_d3d11[] = {
                    AV_PIX_FMT_D3D11,
                    AV_PIX_FMT_NONE,
                };
                output_pix_fmts = output_pix_fmts_d3d11;
            }
            break;
    #endif
    #if CONFIG_DXVA2
        case AV_HWDEVICE_TYPE_DXVA2:
            {
                static const enum AVPixelFormat output_pix_fmts_dxva2[] = {
                    AV_PIX_FMT_DXVA2_VLD,
                    AV_PIX_FMT_NONE,
                };
                output_pix_fmts = output_pix_fmts_dxva2;
            }
            break;
    #endif
        default:
            {
                av_log(avctx, AV_LOG_ERROR, "Unsupported device : %s\n", av_hwdevice_get_type_name(device_ctx->type));
                return AVERROR(EINVAL);
            }
            break;
        }
    }

    input_formats = ff_make_format_list(output_pix_fmts);
    if (!input_formats) {
        return AVERROR(ENOMEM);
    }

    for (i = 0; input_pix_fmts[i] != AV_PIX_FMT_NONE; i++) {
        err = ff_add_format(&input_formats, input_pix_fmts[i]);
        if (err < 0)
            return err;
    }

    if ((err = ff_formats_ref(input_formats, &avctx->inputs[0]->out_formats)) < 0 ||
        (err = ff_formats_ref(ff_make_format_list(output_pix_fmts),
                              &avctx->outputs[0]->in_formats)) < 0)
        return err;

    return 0;
}

static int amf_scale_config_output(AVFilterLink *outlink)
{
    AVFilterContext *avctx = outlink->src;
    AVFilterLink   *inlink = avctx->inputs[0];
    AMFScaleContext  *ctx = avctx->priv;
    AVAMFDeviceContext *amf_ctx;
    AVHWFramesContext *hwframes_out;
    enum AVPixelFormat pix_fmt_in;
    AMFSize out_size;
    int err;
    AMF_RESULT res;

    if ((err = ff_scale_eval_dimensions(avctx,
                                        ctx->w_expr, ctx->h_expr,
                                        inlink, outlink,
                                        &ctx->width, &ctx->height)) < 0)
        return err;

    av_buffer_unref(&ctx->amf_device_ref);
    av_buffer_unref(&ctx->hwframes_in_ref);
    av_buffer_unref(&ctx->hwframes_out_ref);

    if (inlink->hw_frames_ctx) {
        AVHWFramesContext *frames_ctx = (AVHWFramesContext*)inlink->hw_frames_ctx->data;

        if (amf_av_to_amf_format(frames_ctx->sw_format) == AMF_SURFACE_UNKNOWN) {
            av_log(avctx, AV_LOG_ERROR, "Format of input frames context (%s) is not supported by AMF.\n",
                   av_get_pix_fmt_name(frames_ctx->sw_format));
            return AVERROR(EINVAL);
        }

        err = av_hwdevice_ctx_create_derived(&ctx->amf_device_ref, AV_HWDEVICE_TYPE_AMF, frames_ctx->device_ref, 0);
        if (err < 0)
            return err;

        ctx->hwframes_in_ref = av_buffer_ref(inlink->hw_frames_ctx);
        if (!ctx->hwframes_in_ref)
            return AVERROR(ENOMEM);

        ctx->hwframes_out_ref = av_hwframe_ctx_alloc(frames_ctx->device_ref);
        if (!ctx->hwframes_out_ref)
            return AVERROR(ENOMEM);

        hwframes_out = (AVHWFramesContext*)ctx->hwframes_out_ref->data;
        hwframes_out->format    = outlink->format;
        hwframes_out->sw_format = frames_ctx->sw_format;
        pix_fmt_in = frames_ctx->sw_format;

    } else if (avctx->hw_device_ctx) {
        err = av_hwdevice_ctx_create_derived(&ctx->amf_device_ref, AV_HWDEVICE_TYPE_AMF, avctx->hw_device_ctx, 0);
        if (err < 0)
            return err;

        ctx->hwdevice_ref = av_buffer_ref(avctx->hw_device_ctx);
        if (!ctx->hwdevice_ref)
            return AVERROR(ENOMEM);

        ctx->hwframes_out_ref = av_hwframe_ctx_alloc(ctx->hwdevice_ref);
        if (!ctx->hwframes_out_ref)
            return AVERROR(ENOMEM);

        hwframes_out = (AVHWFramesContext*)ctx->hwframes_out_ref->data;
        hwframes_out->format    = outlink->format;
        hwframes_out->sw_format = inlink->format;
        pix_fmt_in = inlink->format;

    } else {
        av_log(ctx, AV_LOG_ERROR, "A hardware device reference is required to init hwcontext_amf.\n");
        return AVERROR(EINVAL);
    }

    if (ctx->format != AV_PIX_FMT_NONE) {
        hwframes_out->sw_format = ctx->format;
    }

    outlink->w = ctx->width;
    outlink->h = ctx->height;

    hwframes_out->width = outlink->w;
    hwframes_out->height = outlink->h;

    err = av_hwframe_ctx_init(ctx->hwframes_out_ref);
    if (err < 0)
        return err;

    outlink->hw_frames_ctx = av_buffer_ref(ctx->hwframes_out_ref);
    if (!outlink->hw_frames_ctx) {
        return AVERROR(ENOMEM);
    }

    amf_ctx = ((AVHWDeviceContext*)ctx->amf_device_ref->data)->hwctx;
    ctx->context = amf_ctx->context;
    ctx->factory = amf_ctx->factory;

    res = ctx->factory->pVtbl->CreateComponent(ctx->factory, ctx->context, AMFVideoConverter, &ctx->converter);
    AMF_RETURN_IF_FALSE(ctx, res == AMF_OK, AVERROR_FILTER_NOT_FOUND, "CreateComponent(%ls) failed with error %d\n", AMFVideoConverter, res);

    AMF_ASSIGN_PROPERTY_INT64(res, ctx->converter, AMF_VIDEO_CONVERTER_OUTPUT_FORMAT, (amf_int32)amf_av_to_amf_format(hwframes_out->sw_format));
    AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "AMFConverter-SetProperty() failed with error %d\n", res);

    out_size.width = outlink->w;
    out_size.height = outlink->h;
    AMF_ASSIGN_PROPERTY_SIZE(res, ctx->converter, AMF_VIDEO_CONVERTER_OUTPUT_SIZE, out_size);
    AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "AMFConverter-SetProperty() failed with error %d\n", res);

    AMF_ASSIGN_PROPERTY_INT64(res, ctx->converter, AMF_VIDEO_CONVERTER_SCALE, (amf_int32)ctx->scale_type);
    AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "AMFConverter-SetProperty() failed with error %d\n", res);


    res = ctx->converter->pVtbl->Init(ctx->converter, amf_av_to_amf_format(pix_fmt_in), inlink->w, inlink->h);
    AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "AMFConverter-Init() failed with error %d\n", res);

    return 0;
}

static int amf_scale_filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext             *avctx = inlink->dst;
    AMFScaleContext             *ctx = avctx->priv;
    AVFilterLink                *outlink = avctx->outputs[0];
    AMF_RESULT  res;
    AMFSurface *surface_in;
    AMFSurface *surface_out;
    AMFData *data_out;
    enum AMF_VIDEO_CONVERTER_COLOR_PROFILE_ENUM amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_UNKNOWN;

    AVFrame *out = NULL;
    int ret = 0;

    if (!ctx->converter)
        return AVERROR(EINVAL);

    ret = amf_avframe_to_amfsurface(avctx, in, &surface_in);
    if (ret < 0)
        goto fail;

    res = ctx->converter->pVtbl->SubmitInput(ctx->converter, (AMFData*)surface_in);
    AMFAV_GOTO_FAIL_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "SubmitInput() failed with error %d\n", res);

    res = ctx->converter->pVtbl->QueryOutput(ctx->converter, &data_out);
    AMFAV_GOTO_FAIL_IF_FALSE(avctx, res == AMF_OK, AVERROR_UNKNOWN, "QueryOutput() failed with error %d\n", res);

    if (data_out) {
        AMFGuid guid = IID_AMFSurface();
        data_out->pVtbl->QueryInterface(data_out, &guid, (void**)&surface_out); // query for buffer interface
        data_out->pVtbl->Release(data_out);
    }

    out = amf_amfsurface_to_avframe(avctx, surface_out);

    ret = av_frame_copy_props(out, in);
    if (ret < 0)
        goto fail;

    switch(in->colorspace) {
    case AVCOL_SPC_BT470BG:
    case AVCOL_SPC_SMPTE170M:
    case AVCOL_SPC_SMPTE240M:
        amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_601;
        break;
    case AVCOL_SPC_BT709:
        amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_709;
        break;
    case AVCOL_SPC_BT2020_NCL:
    case AVCOL_SPC_BT2020_CL:
        amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_2020;
        break;
    case AVCOL_SPC_RGB:
        amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_JPEG;
        break;
    default:
        amf_color_profile = AMF_VIDEO_CONVERTER_COLOR_PROFILE_UNKNOWN;
        break;
    }

    if (amf_color_profile != AMF_VIDEO_CONVERTER_COLOR_PROFILE_UNKNOWN) {
        AMF_ASSIGN_PROPERTY_INT64(res, ctx->converter, AMF_VIDEO_CONVERTER_COLOR_PROFILE, amf_color_profile);
    }

    out->format = outlink->format;
    out->width  = outlink->w;
    out->height = outlink->h;

    out->hw_frames_ctx = av_buffer_ref(ctx->hwframes_out_ref);
    if (!out->hw_frames_ctx) {
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    surface_in->pVtbl->Release(surface_in);
    surface_out->pVtbl->Release(surface_out);

    if (inlink->sample_aspect_ratio.num) {
        outlink->sample_aspect_ratio = av_mul_q((AVRational){outlink->h * inlink->w, outlink->w * inlink->h}, inlink->sample_aspect_ratio);
    } else
        outlink->sample_aspect_ratio = inlink->sample_aspect_ratio;

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
fail:
    av_frame_free(&in);
    av_frame_free(&out);
    return ret;
}

#define OFFSET(x) offsetof(AMFScaleContext, x)
#define FLAGS AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_FILTERING_PARAM
static const AVOption scale_amf_options[] = {
    { "w",      "Output video width",               OFFSET(w_expr),     AV_OPT_TYPE_STRING, { .str = "iw"   }, .flags = FLAGS },
    { "h",      "Output video height",              OFFSET(h_expr),     AV_OPT_TYPE_STRING, { .str = "ih"   }, .flags = FLAGS },
    { "format", "Output pixel format",              OFFSET(format_str), AV_OPT_TYPE_STRING, { .str = "same" }, .flags = FLAGS },

    { "scale_type",    "Scale type",                OFFSET(scale_type),      AV_OPT_TYPE_INT,   { .i64 = AMF_VIDEO_CONVERTER_SCALE_BILINEAR },
        AMF_VIDEO_CONVERTER_SCALE_BILINEAR, AMF_VIDEO_CONVERTER_SCALE_BICUBIC, FLAGS, "scale_type" },
    { "bilinear",      "Bilinear",      0,                       AV_OPT_TYPE_CONST, { .i64 = AMF_VIDEO_CONVERTER_SCALE_BILINEAR }, 0, 0, FLAGS, "scale_type" },
    { "bicubic",       "Bicubic",       0,                       AV_OPT_TYPE_CONST, { .i64 = AMF_VIDEO_CONVERTER_SCALE_BICUBIC },  0, 0, FLAGS, "scale_type" },

    { NULL },
};


AVFILTER_DEFINE_CLASS(scale_amf);

static const AVFilterPad amf_scale_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = amf_scale_filter_frame,
    },
    { NULL }
};

static const AVFilterPad amf_scale_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = amf_scale_config_output,
    },
    { NULL }
};

AVFilter ff_vf_scale_amf = {
    .name      = "scale_amf",
    .description = NULL_IF_CONFIG_SMALL("AMF video scaling and format conversion"),

    .init          = amf_scale_init,
    .uninit        = amf_scale_uninit,
    .query_formats = amf_scale_query_formats,

    .priv_size = sizeof(AMFScaleContext),
    .priv_class = &scale_amf_class,

    .inputs    = amf_scale_inputs,
    .outputs   = amf_scale_outputs,

    .flags_internal = FF_FILTER_FLAG_HWFRAME_AWARE,
};
