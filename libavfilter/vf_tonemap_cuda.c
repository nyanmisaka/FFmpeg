/*
 * Copyright (c) 2021 nyanmisaka <nst799610810@gmail.com>
 *
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
 * Perform HDR to SDR conversion with tonemapping using cuda hardware acceleration
 */

#include "libavutil/log.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_cuda_internal.h"
#include "libavutil/cuda_check.h"

#include "avfilter.h"
#include "internal.h"
#include "colorspace.h"

#define CHECK_CU(x) FF_CUDA_CHECK_DL(ctx, ctx->hwctx->internal->cuda_dl, x)
#define DIV_UP(a, b) ( ((a) + (b) - 1) / (b) )

#define BLOCK_X 32
#define BLOCK_Y 16

static const enum AVPixelFormat supported_formats[] = {
    AV_PIX_FMT_NV12,
    AV_PIX_FMT_P010,
    AV_PIX_FMT_NONE,
};

/**
 * TonemapCUDAContext
 */
typedef struct TonemapCUDAContext {
    const AVClass      *class;

    enum AVColorSpace colorspace, colorspace_in, colorspace_out;
    double yuv2rgb[3][3], rgb2yuv[3][3], rgb2rgb[3][3];

    enum AVPixelFormat in_fmt;
    enum AVPixelFormat out_fmt;

    AVCUDADeviceContext *hwctx;

    CUcontext cu_ctx;
    CUmodule cu_module;
    CUfunction cu_func;
    CUstream cu_stream;

} TonemapCUDAContext;

static const struct PrimaryCoefficients primaries_table[AVCOL_PRI_NB] = {
    [AVCOL_PRI_BT709]  = { 0.640, 0.330, 0.300, 0.600, 0.150, 0.060 },
    [AVCOL_PRI_BT2020] = { 0.708, 0.292, 0.170, 0.797, 0.131, 0.046 },
};

static const struct WhitepointCoefficients whitepoint_table[AVCOL_PRI_NB] = {
    [AVCOL_PRI_BT709]  = { 0.3127, 0.3290 },
    [AVCOL_PRI_BT2020] = { 0.3127, 0.3290 },
};

static void get_rgb2rgb_matrix(enum AVColorPrimaries in, enum AVColorPrimaries out,
                               double rgb2rgb[3][3]) {
    double rgb2xyz[3][3], xyz2rgb[3][3];

    ff_fill_rgb2xyz_table(&primaries_table[out], &whitepoint_table[out], rgb2xyz);
    ff_matrix_invert_3x3(rgb2xyz, xyz2rgb);
    ff_fill_rgb2xyz_table(&primaries_table[in], &whitepoint_table[in], rgb2xyz);
    ff_matrix_mul_3x3(rgb2rgb, rgb2xyz, xyz2rgb);
}

/**
 * Helper to find out if provided format is supported by filter
 */
static int format_is_supported(const enum AVPixelFormat formats[], enum AVPixelFormat fmt)
{
    for (int i = 0; formats[i] != AV_PIX_FMT_NONE; i++)
        if (formats[i] == fmt)
            return 1;
    return 0;
}

static void test(double rgb2yuv[3][3])
{
    av_log(NULL, AV_LOG_INFO, "rgb2yuv[0][0]: %f\n\n", rgb2yuv[0][0]);
}

static int tonemap_cuda_filter_frame(AVFilterLink *inlink, AVFrame *input)
{
    int ret;

    AVFilterContext *avctx = inlink->dst;
    TonemapCUDAContext *ctx = avctx->priv;
    AVFilterLink *outlink = avctx->outputs[0];

    CudaFunctions *cu = ctx->hwctx->internal->cuda_dl;
    CUcontext dummy, cuda_ctx = ctx->hwctx->cuda_ctx;

    ctx->cu_ctx = cuda_ctx;

    //AVFrame *output = NULL;
    //output = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    //if (!output)
    //{
    //    ret = AVERROR(ENOMEM);
    //    return ret;
    //}

    //ret = av_frame_copy_props(output, input);
    //if (ret < 0) {
    //    av_frame_free(&input);
    //    av_frame_free(&output);
    //    return ret;
    //}

    //if (ctx->colorspace != -1)
    //    output->colorspace = ctx->colorspace;

    //ctx->colorspace_in = input->colorspace;
    //ctx->colorspace_out = output->colorspace;
    
	ret = av_frame_make_writable(input);
    if (ret < 0) {
        av_frame_free(&input);
        //av_frame_free(&output);
        return ret;
    }
    
    // push cuda context

    ret = CHECK_CU(cu->cuCtxPushCurrent(cuda_ctx));
    if (ret < 0) {
        av_frame_free(&input);
        return ret;
    }

    //test(ctx->rgb2yuv);
    //av_log(NULL, AV_LOG_INFO, "1--------------%f\n\n", ctx->rgb2yuv);
    //av_log(NULL, AV_LOG_INFO, "2--------------%f\n\n", ctx->rgb2yuv[0]);
    //av_log(NULL, AV_LOG_INFO, "3--------------%f\n\n", ctx->rgb2yuv[0][0]);
    //av_log(avctx, AV_LOG_INFO, "rgb2yuv[0][0]: %f\n\n", ctx->rgb2yuv[0][0]);
    //av_log(avctx, AV_LOG_INFO, "rgb2yuv[0][1]: %f\n\n", ctx->rgb2yuv[0][1]);
    //av_log(avctx, AV_LOG_INFO, "rgb2yuv[0][2]: %f\n\n", ctx->rgb2yuv[0][2]);

    CUdeviceptr pyuv2rgb, prgb2yuv, prgb2rgb;
    CHECK_CU(cu->cuMemAlloc(&pyuv2rgb, 3 * 3 * sizeof(double)));
    CHECK_CU(cu->cuMemAlloc(&prgb2yuv, 3 * 3 * sizeof(double)));
    CHECK_CU(cu->cuMemAlloc(&prgb2rgb, 3 * 3 * sizeof(double)));

    CUDA_MEMCPY2D cpy1 = {
        .srcMemoryType = CU_MEMORYTYPE_HOST,
        .dstMemoryType = CU_MEMORYTYPE_DEVICE,
        .srcHost       = ctx->yuv2rgb,
        .dstDevice     = pyuv2rgb,
        .srcPitch      = 3 * sizeof(double),
        .dstPitch      = 3 * sizeof(double),
        .WidthInBytes  = 3 * sizeof(double),
        .Height        = 3,
    };

    CUDA_MEMCPY2D cpy2 = {
        .srcMemoryType = CU_MEMORYTYPE_HOST,
        .dstMemoryType = CU_MEMORYTYPE_DEVICE,
        .srcHost       = ctx->rgb2yuv,
        .dstDevice     = prgb2yuv,
        .srcPitch      = 3 * sizeof(double),
        .dstPitch      = 3 * sizeof(double),
        .WidthInBytes  = 3 * sizeof(double),
        .Height        = 3,
    };

    CUDA_MEMCPY2D cpy3 = {
        .srcMemoryType = CU_MEMORYTYPE_HOST,
        .dstMemoryType = CU_MEMORYTYPE_DEVICE,
        .srcHost       = ctx->rgb2rgb,
        .dstDevice     = prgb2rgb,
        .srcPitch      = 3 * sizeof(double),
        .dstPitch      = 3 * sizeof(double),
        .WidthInBytes  = 3 * sizeof(double),
        .Height        = 3,
    };
    
    CHECK_CU(cu->cuMemcpy2D(&cpy1));
    CHECK_CU(cu->cuMemcpy2D(&cpy2));
    CHECK_CU(cu->cuMemcpy2D(&cpy3));

    //av_log(NULL, AV_LOG_INFO, "linesize0: %d, linesize1: %d\n\n", input->linesize[0], input->linesize[1]);
    
    void* kernel_args[] = {
        &input->data[0], &input->linesize[0],
        &input->data[1], &input->linesize[1],
        &input->width, &input->height,
        &pyuv2rgb, &prgb2yuv, &prgb2rgb,
    };

    CHECK_CU(cu->cuLaunchKernel(
        ctx->cu_func,
        DIV_UP(input->width, BLOCK_X), DIV_UP(input->height, BLOCK_Y), 1,
        BLOCK_X, BLOCK_Y, 1,
        0, ctx->cu_stream, kernel_args, NULL));
    
    
    
    //CHECK_CU(cu->cuMemFree(pyuv2rgb));
    //CHECK_CU(cu->cuMemFree(prgb2yuv));
    
    CHECK_CU(cu->cuCtxPopCurrent(&dummy));
    
    return ff_filter_frame(outlink, input);
}

/**
 * Initialize tonemap_cuda
 */
static av_cold int tonemap_cuda_init(AVFilterContext *avctx)
{
    av_log(avctx, AV_LOG_INFO, "tonemap_cuda_init\n\n");
    TonemapCUDAContext* ctx = avctx->priv;
    const struct LumaCoefficients *luma_src, *luma_dst;

    ctx->colorspace_in = AVCOL_SPC_BT2020_NCL;
    ctx->colorspace_out = AVCOL_SPC_BT2020_NCL;

    luma_src = ff_get_luma_coefficients(ctx->colorspace_in);
    if (!luma_src) {
        av_log(avctx, AV_LOG_ERROR, "unsupported input colorspace %d (%s)\n",
               ctx->colorspace_in, av_color_space_name(ctx->colorspace_in));
        return AVERROR(EINVAL);
    }

    luma_dst = ff_get_luma_coefficients(ctx->colorspace_out);
    if (!luma_dst) {
        av_log(avctx, AV_LOG_ERROR, "unsupported output colorspace %d (%s)\n",
               ctx->colorspace_out, av_color_space_name(ctx->colorspace_out));
        return AVERROR(EINVAL);
    }

    // fill matrix
    ff_fill_rgb2yuv_table(luma_src, ctx->rgb2yuv);
    ff_matrix_invert_3x3(ctx->rgb2yuv, ctx->yuv2rgb);
    
    get_rgb2rgb_matrix(AVCOL_SPC_BT2020_NCL, AVCOL_SPC_BT2020_NCL, ctx->rgb2rgb);
    
    return 0;
}

/**
 * Uninitialize tonemap_cuda
 */
static av_cold void tonemap_cuda_uninit(AVFilterContext *avctx)
{
    TonemapCUDAContext* ctx = avctx->priv;

    if (ctx->hwctx && ctx->cu_module) {
        CUcontext dummy;
        CudaFunctions *cu = ctx->hwctx->internal->cuda_dl;
        CHECK_CU(cu->cuCtxPushCurrent(ctx->cu_ctx));
        CHECK_CU(cu->cuModuleUnload(ctx->cu_module));
        CHECK_CU(cu->cuCtxPopCurrent(&dummy));
    }
}

/**
 * Query formats
 */
static int tonemap_cuda_query_formats(AVFilterContext *avctx)
{
    static const enum AVPixelFormat pixel_formats[] = {
        AV_PIX_FMT_CUDA, AV_PIX_FMT_NONE,
    };

    AVFilterFormats *pix_fmts = ff_make_format_list(pixel_formats);

    return ff_set_common_formats(avctx, pix_fmts);
}

/**
 * Configure output
 */
static int tonemap_cuda_config_output(AVFilterLink *outlink)
{
    extern char vf_tonemap_cuda_ptx[];

    int err;
    AVFilterContext* avctx = outlink->src;
    TonemapCUDAContext* ctx = avctx->priv;

    AVFilterLink *inlink = avctx->inputs[0];
    AVHWFramesContext  *frames_ctx = (AVHWFramesContext*)inlink->hw_frames_ctx->data;

    CUcontext dummy, cuda_ctx;
    CudaFunctions *cu;

    // check input formats

    if (!frames_ctx) {
        av_log(ctx, AV_LOG_ERROR, "No hw context provided\n");
        return AVERROR(EINVAL);
    }

    ctx->in_fmt = frames_ctx->sw_format;
    if (!format_is_supported(supported_formats, ctx->in_fmt)) {
        av_log(ctx, AV_LOG_ERROR, "Unsupported input format: %s\n",
               av_get_pix_fmt_name(ctx->in_fmt));
        return AVERROR(ENOSYS);
    }

    // initialize

    ctx->hwctx = frames_ctx->device_ctx->hwctx;
    cuda_ctx = ctx->hwctx->cuda_ctx;

    ctx->cu_stream = ctx->hwctx->stream;

    outlink->hw_frames_ctx = av_buffer_ref(inlink->hw_frames_ctx);

    // load functions

    cu = ctx->hwctx->internal->cuda_dl;

    err = CHECK_CU(cu->cuCtxPushCurrent(cuda_ctx));
    if (err < 0) {
        return err;
    }

    err = CHECK_CU(cu->cuModuleLoadData(&ctx->cu_module, vf_tonemap_cuda_ptx));
    if (err < 0) {
        CHECK_CU(cu->cuCtxPopCurrent(&dummy));
        return err;
    }

    err = CHECK_CU(cu->cuModuleGetFunction(&ctx->cu_func, ctx->cu_module, "Tonemap_Cuda"));
    if (err < 0) {
        CHECK_CU(cu->cuCtxPopCurrent(&dummy));
        return err;
    }

    CHECK_CU(cu->cuCtxPopCurrent(&dummy));

    return 0;
}


#define OFFSET(x) offsetof(TonemapCUDAContext, x)
#define FLAGS (AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_VIDEO_PARAM)

static const AVOption tonemap_cuda_options[] = {
    { "matrix", "set colorspace matrix", OFFSET(colorspace), AV_OPT_TYPE_INT, {.i64 = -1}, -1, INT_MAX, FLAGS, "matrix" },
    { "m", "set colorspace matrix", OFFSET(colorspace), AV_OPT_TYPE_INT, {.i64 = -1}, -1, INT_MAX, FLAGS, "matrix" },
    {     "bt709",            0,       0,                 AV_OPT_TYPE_CONST, {.i64 = AVCOL_SPC_BT709},         0, 0, FLAGS, "matrix" },
    {     "bt2020",           0,       0,                 AV_OPT_TYPE_CONST, {.i64 = AVCOL_SPC_BT2020_NCL},    0, 0, FLAGS, "matrix" },
    { NULL },
};

static const AVClass tonemap_cuda_class = {
    .class_name = "tonemap_cuda",
    .item_name  = av_default_item_name,
    .option     = tonemap_cuda_options,
    .version    = LIBAVUTIL_VERSION_INT,
    .category   = AV_CLASS_CATEGORY_FILTER,
};

static const AVFilterPad tonemap_cuda_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = &tonemap_cuda_filter_frame,
    },
    { NULL }
};

static const AVFilterPad tonemap_cuda_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = &tonemap_cuda_config_output,
    },
    { NULL }
};

AVFilter ff_vf_tonemap_cuda = {
    .name            = "tonemap_cuda",
    .description     = NULL_IF_CONFIG_SMALL("Perform HDR to SDR conversion with tonemapping using CUDA"),
    .priv_size       = sizeof(TonemapCUDAContext),
    .priv_class      = &tonemap_cuda_class,
    .init            = &tonemap_cuda_init,
    .uninit          = &tonemap_cuda_uninit,
    .query_formats   = &tonemap_cuda_query_formats,
    .inputs          = tonemap_cuda_inputs,
    .outputs         = tonemap_cuda_outputs,
    .flags_internal  = FF_FILTER_FLAG_HWFRAME_AWARE,
};
