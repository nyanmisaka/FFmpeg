/*
 * Copyright (c) 2018 Gabriel Machado
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

#include "libavutil/common.h"
#include "libavutil/imgutils.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "avfilter.h"
#include "internal.h"
#include "opencl.h"
#include "opencl_source.h"
#include "scale_eval.h"
#include "video.h"

enum filters {
    F_AREA,
    F_BICUBIC,
    F_BILINEAR,
    F_GAUSSIAN,
    F_LANCZOS,
    F_NEIGHBOR,
    F_SINC,
    F_SPLINE,
    F_EXPERIMENTAL
};

static int filter_radius[] = {
    [F_AREA]         =  1,
    [F_BICUBIC]      =  2,
    [F_BILINEAR]     =  1,
    [F_GAUSSIAN]     =  4,
    [F_LANCZOS]      =  3,
    [F_NEIGHBOR]     = -1,
    [F_SINC]         =  10,
    [F_SPLINE]       =  10,
    [F_EXPERIMENTAL] =  4
};

typedef struct ScaleOpenCLContext {
    OpenCLFilterContext ocf;

    cl_command_queue command_queue;
    cl_mem           cx, cy;
    cl_kernel        kernel;
    const char      *kernel_name;

    char *w_expr,  *h_expr;
    int   dst_w,    dst_h;
    int   src_w,    src_h;
    int   algorithm;
    int   force_original_aspect_ratio;
    int   force_divisible_by;
    enum AVPixelFormat format;

    cl_int2   flt_size;
    int       initialised;
} ScaleOpenCLContext;

static float netravali(float t, float B, float C)
{
    if (t > 2) {
        return 0;
    } else {
        float tt  = t*t;
        float ttt = t*tt;
        if (t < 1) {
            return ((12 -  9 * B - 6 * C) * ttt +
                   (-18 + 12 * B + 6 * C) * tt  +
                     (6 -  2 * B)) / 6;
        } else {
            return     ((-B -  6 * C) * ttt +
                     (6 * B + 30 * C) * tt +
                   (-12 * B - 48 * C) * t +
                     (8 * B + 24 * C)) / 6;
        }
    }
}

static float sinc(float t)
{
    return (t == 0) ? 1.0 : sin(t * M_PI) / (t * M_PI);
}

static float lanczos(float t, float a)
{
    return (t < a) ? sinc(t) * sinc(t / a) : 0;
}

static double spline(double a, double b, double c, double d, double dist)
{
    if (dist <= 1.0)
        return ((d * dist + c) * dist + b) * dist + a;
    else
        return spline(0.0,
                      b + 2.0 * c + 3.0 * d,
                      c + 3.0 * d,
                      -b - 3.0 * c - 6.0 * d,
                      dist - 1.0);
}

static float calc_weight(int algorithm, float ratio, float t)
{
    t = fabs(t);

    switch (algorithm) {
        case F_AREA: {
            float t2 = t - 0.5;
            if (t2 * ratio < -0.5)
                return 1;
            else if (t2 * ratio < 0.5)
                return -t2 * ratio + 0.5;
            else
                return 0;
        }

        case F_BICUBIC: {
            const float B = 0, C = 0.6;
            return netravali(t, B, C);
        }

        case F_BILINEAR:
            return t < 1 ? (1 - t) : 0;

        case F_EXPERIMENTAL: {
            double A = 1.0;
            double c;

            if (t < 1.0)
                c = cos(t * M_PI);
            else
                c = -1.0;
            if (c < 0.0)
                c = -pow(-c, A);
            else
                c = pow(c, A);
            return c * 0.5 + 0.5;
        }

        case F_GAUSSIAN: {
            const float p = 3.0;
            return exp2(-p * t * t);
        }

        case F_LANCZOS: {
            return lanczos(t, filter_radius[algorithm]);
        }

        case F_NEIGHBOR:
            return 1;

        case F_SINC:
            return sinc(t);

        case F_SPLINE: {
            const double p = -2.196152422706632;
            return spline(1.0, 0.0, p, -p - 1.0, t);
        }
    }

    return 0;
}

static int scale_opencl_init(AVFilterContext *avctx)
{
    ScaleOpenCLContext *ctx = avctx->priv;
    cl_int cle;
    int err;
    int i, j;
    int filterw, filterh;
    float scalex, scaley;
    float *cx = NULL, *cy = NULL;

    if (ctx->algorithm == F_NEIGHBOR) {
        ctx->kernel_name = "neighbor";
    } else {
        ctx->kernel_name = "scale";

        scalex = FFMAX((float) ctx->src_w / ctx->dst_w, 1);
        scaley = FFMAX((float) ctx->src_h / ctx->dst_h, 1);
        filterw = ceil(2 * filter_radius[ctx->algorithm] * scalex);
        filterh = ceil(2 * filter_radius[ctx->algorithm] * scaley);

        filterw = FFMIN(filterw, ctx->src_w - 2);
        filterw = FFMAX(filterw, 1);
        filterh = FFMIN(filterh, ctx->src_h - 2);
        filterh = FFMAX(filterh, 1);

        ctx->flt_size.s[0] = filterw;
        ctx->flt_size.s[1] = filterh;

        av_log(avctx, AV_LOG_DEBUG, "filter size: %d %d.\n", filterw, filterh);

        cx = av_malloc_array(ctx->dst_w * filterw, sizeof(cl_float));
        cy = av_malloc_array(ctx->dst_h * filterh, sizeof(cl_float));

        if (!cx || !cy) {
            err = AVERROR(ENOMEM);
            goto fail;
        }

        for (i = 0; i < ctx->dst_w; ++i) {
            float s_x = (i + 0.5) * ctx->src_w / ctx->dst_w - 0.5;
            float t = s_x - floor(s_x);  // fract

            float sum = 0;
            for (j = 0; j < filterw; ++j) {
                int x = filterw/2 - j;
                sum += cx[i * filterw + j] = calc_weight(ctx->algorithm,
                                                         scalex,
                                                         (x - t) / scalex);
            }

            for (j = 0; j < filterw; ++j)
                cx[i * filterw + j] /= sum;
        }

        for (i = 0; i < ctx->dst_h; ++i) {
            float s_y = (i + 0.5) * ctx->src_h / ctx->dst_h - 0.5;
            float t = s_y - floor(s_y);  // fract

            float sum = 0;
            for (j = 0; j < filterh; ++j) {
                int y = filterh/2 - j;
                sum += cy[i * filterh + j] = calc_weight(ctx->algorithm,
                                                         scaley,
                                                         (y - t) / scaley);
            }

            for (j = 0; j < filterh; ++j)
                cy[i * filterh + j] /= sum;
        }

        ctx->cx = clCreateBuffer(ctx->ocf.hwctx->context,
                                 CL_MEM_READ_ONLY     |
                                 CL_MEM_COPY_HOST_PTR |
                                 CL_MEM_HOST_NO_ACCESS,
                                 ctx->dst_w * filterw * sizeof(cl_float),
                                 cx,
                                 &cle);

        ctx->cy = clCreateBuffer(ctx->ocf.hwctx->context,
                                 CL_MEM_READ_ONLY     |
                                 CL_MEM_COPY_HOST_PTR |
                                 CL_MEM_HOST_NO_ACCESS,
                                 ctx->dst_h * filterh * sizeof(cl_float),
                                 cy,
                                 &cle);
        av_free(cx);
        av_free(cy);
        if (!ctx->cx || !ctx->cy) {
            av_log(avctx, AV_LOG_ERROR, "Failed to create weights buffer: %d.\n", cle);
            err = AVERROR(EIO);
            goto fail;
        }
    }

    err = ff_opencl_filter_load_program(avctx, &ff_opencl_source_scale, 1);
    if (err < 0)
        goto fail;

    ctx->command_queue = clCreateCommandQueue(ctx->ocf.hwctx->context,
                                              ctx->ocf.hwctx->device_id,
                                              0, &cle);
    if (!ctx->command_queue) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create OpenCL command queue: %d.\n", cle);
        err = AVERROR(EIO);
        goto fail;
    }

    ctx->kernel = clCreateKernel(ctx->ocf.program, ctx->kernel_name, &cle);
    if (!ctx->kernel) {
        av_log(avctx, AV_LOG_ERROR, "Failed to create kernel: %d.\n", cle);
        err = AVERROR(EIO);
        goto fail;
    }

    ctx->initialised = 1;

    return 0;

fail:
    if (ctx->command_queue)
        clReleaseCommandQueue(ctx->command_queue);
    if (ctx->kernel)
        clReleaseKernel(ctx->kernel);
    if (ctx->cx)
        clReleaseMemObject(ctx->cx);
    if (ctx->cy)
        clReleaseMemObject(ctx->cy);
    if (cx)
        av_free(cx);
    if (cy)
        av_free(cy);
    return err;
}

static int config_input_props(AVFilterLink *inlink)
{
    AVFilterContext  *avctx = inlink->dst;
    AVFilterLink   *outlink = avctx->outputs[0];
    ScaleOpenCLContext *ctx = avctx->priv;
    int ret;

    if ((ret = ff_scale_eval_dimensions(ctx,
                                        ctx->w_expr, ctx->h_expr,
                                        inlink, outlink,
                                        &ctx->dst_w, &ctx->dst_h)) < 0)
        return ret;

    ff_scale_adjust_dimensions(inlink, &ctx->dst_w, &ctx->dst_h,
                               ctx->force_original_aspect_ratio, ctx->force_divisible_by);

    if (((int64_t) ctx->dst_h * inlink->w) > INT_MAX  ||
        ((int64_t) ctx->dst_w * inlink->h) > INT_MAX)
        av_log(ctx, AV_LOG_ERROR, "Rescaled value for width or height is too big.\n");

    ctx->ocf.output_width  = ctx->dst_w;
    ctx->ocf.output_height = ctx->dst_h;
    ctx->src_w = inlink->w;
    ctx->src_h = inlink->h;

    return ff_opencl_filter_config_input(inlink);
}

static int config_output_props(AVFilterLink *outlink)
{
    AVFilterContext  *avctx = outlink->src;
    ScaleOpenCLContext *ctx = avctx->priv;
    int ret;

    if (ctx->format != AV_PIX_FMT_NONE) {
        if (ctx->format == AV_PIX_FMT_YUV420P ||
            ctx->format == AV_PIX_FMT_YUV420P16 ||
            ctx->format == AV_PIX_FMT_YUV444P ||
            ctx->format == AV_PIX_FMT_YUV444P16 ||
            ctx->format == AV_PIX_FMT_NV12 ||
            ctx->format == AV_PIX_FMT_P010 ||
            ctx->format == AV_PIX_FMT_P016) {
            ctx->ocf.output_format = ctx->format;
        } else {
            av_log(avctx, AV_LOG_ERROR, "Unsupported output format,"
                   "only nv12|p010|p016|yuv420p|yuv420p16|yuv444p|yuv444p16 are supported now\n");
            return AVERROR(EINVAL);
        }
    }

    ret = ff_opencl_filter_config_output(outlink);
    if (ret < 0)
        return ret;

    return 0;
}

static int scale_opencl_filter_frame(AVFilterLink *inlink, AVFrame *input)
{
    AVFilterContext     *avctx = inlink->dst;
    AVFilterLink      *outlink = avctx->outputs[0];
    ScaleOpenCLContext    *ctx = avctx->priv;
    AVHWFramesContext *main_fc = (AVHWFramesContext *)inlink->hw_frames_ctx->data;
    const AVPixFmtDescriptor *pix_desc = av_pix_fmt_desc_get(main_fc->sw_format);
    AVFrame *output = NULL;

    cl_int cle;
    size_t global_work[2];
    cl_mem src, dst;
    int err, p;
    int x_subsample = 1 << pix_desc->log2_chroma_w;
    int y_subsample = 1 << pix_desc->log2_chroma_h;

    av_log(ctx, AV_LOG_DEBUG, "Filter input: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(input->format),
           input->width, input->height, input->pts);

    if (!input->hw_frames_ctx)
        return AVERROR(EINVAL);

    if (!(main_fc->sw_format == AV_PIX_FMT_YUV420P ||
        main_fc->sw_format == AV_PIX_FMT_YUV420P16 ||
        main_fc->sw_format == AV_PIX_FMT_YUV444P ||
        main_fc->sw_format == AV_PIX_FMT_YUV444P16 ||
        main_fc->sw_format == AV_PIX_FMT_NV12 ||
        main_fc->sw_format == AV_PIX_FMT_P010 ||
        main_fc->sw_format == AV_PIX_FMT_P016)) {
        av_log(ctx, AV_LOG_ERROR, "Unsupported input format: %s\n",
               av_get_pix_fmt_name(main_fc->sw_format));
        err = AVERROR(ENOSYS);
        goto fail;
    }

    if (!ctx->initialised) {
        err = scale_opencl_init(avctx);
        if (err < 0)
            goto fail;
    }

    output = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!output) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    err = av_frame_copy_props(output, input);
    if (err < 0)
        goto fail;
    output->width  = outlink->w;
    output->height = outlink->h;

    for (p = 0; p < FF_ARRAY_ELEMS(output->data); p++) {
        src = (cl_mem) input->data[p];
        dst = (cl_mem)output->data[p];

        if (!dst)
            break;

        cle = clSetKernelArg(ctx->kernel, 0, sizeof(cl_mem), &dst);
        if (cle != CL_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to set kernel "
                   "destination image argument: %d.\n", cle);
            goto fail;
        }
        cle = clSetKernelArg(ctx->kernel, 1, sizeof(cl_mem), &src);
        if (cle != CL_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to set kernel "
                   "source image argument: %d.\n", cle);
            goto fail;
        }

        if (ctx->algorithm != F_NEIGHBOR) {
            cle = clSetKernelArg(ctx->kernel, 2, sizeof(cl_mem), &ctx->cx);
            if (cle != CL_SUCCESS) {
                av_log(avctx, AV_LOG_ERROR, "Failed to set kernel "
                "cx argument: %d.\n", cle);
                goto fail;
            }
            cle = clSetKernelArg(ctx->kernel, 3, sizeof(cl_mem), &ctx->cy);
            if (cle != CL_SUCCESS) {
                av_log(avctx, AV_LOG_ERROR, "Failed to set kernel "
                "cy argument: %d.\n", cle);
                goto fail;
            }
            cle = clSetKernelArg(ctx->kernel, 4, sizeof(cl_int2), &ctx->flt_size);
            if (cle != CL_SUCCESS) {
                av_log(avctx, AV_LOG_ERROR, "Failed to set kernel "
                "filter size argument: %d.\n", cle);
                goto fail;
            }
        }

        global_work[0] = output->width / (p ? x_subsample : 1);
        global_work[1] = output->height / (p ? y_subsample : 1);

        av_log(avctx, AV_LOG_DEBUG, "Run kernel on plane %d "
               "(%"SIZE_SPECIFIER"x%"SIZE_SPECIFIER").\n",
               p, global_work[0], global_work[1]);

        cle = clEnqueueNDRangeKernel(ctx->command_queue, ctx->kernel, 2, NULL,
                                     global_work, NULL, 0, NULL, NULL);
        if (cle != CL_SUCCESS) {
            av_log(avctx, AV_LOG_ERROR, "Failed to enqueue kernel: %d.\n", cle);
            err = AVERROR(EIO);
            goto fail;
        }
    }

    cle = clFinish(ctx->command_queue);
    if (cle != CL_SUCCESS) {
        av_log(avctx, AV_LOG_ERROR, "Failed to finish command queue: %d.\n", cle);
        err = AVERROR(EIO);
        goto fail;
    }

    av_frame_free(&input);

    av_log(ctx, AV_LOG_DEBUG, "Filter output: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(output->format),
           output->width, output->height, output->pts);

    return ff_filter_frame(outlink, output);

fail:
    clFinish(ctx->command_queue);
    av_frame_free(&input);
    av_frame_free(&output);
    return err;
}

static av_cold void scale_opencl_uninit(AVFilterContext *avctx)
{
    ScaleOpenCLContext *ctx = avctx->priv;
    cl_int cle;

    if (ctx->kernel) {
        cle = clReleaseKernel(ctx->kernel);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
                   "kernel: %d.\n", cle);
    }

    if (ctx->command_queue) {
        cle = clReleaseCommandQueue(ctx->command_queue);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
                   "command queue: %d.\n", cle);
    }

    if (ctx->cx) {
        cle = clReleaseMemObject(ctx->cx);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
            "weights buffer: %d.\n", cle);
    }

    if (ctx->cy) {
        cle = clReleaseMemObject(ctx->cy);
        if (cle != CL_SUCCESS)
            av_log(avctx, AV_LOG_ERROR, "Failed to release "
            "weights buffer: %d.\n", cle);
    }

    ff_opencl_filter_uninit(avctx);
}

#define OFFSET(x) offsetof(ScaleOpenCLContext, x)
#define FLAGS (AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_VIDEO_PARAM)
static const AVOption scale_opencl_options[] = {
    { "w",             "Output video width",  OFFSET(w_expr),    AV_OPT_TYPE_STRING, { .str = "iw"            }, .flags = FLAGS },
    { "h",             "Output video height", OFFSET(h_expr),    AV_OPT_TYPE_STRING, { .str = "ih"            }, .flags = FLAGS },
    { "format",        "Output pixel format", OFFSET(format), AV_OPT_TYPE_PIXEL_FMT, { .i64 = AV_PIX_FMT_NONE }, AV_PIX_FMT_NONE, INT_MAX, FLAGS, "fmt" },
    { "algorithm",     "Scaling algorithm",   OFFSET(algorithm), AV_OPT_TYPE_INT,    { .i64 = F_BILINEAR      }, INT_MIN, INT_MAX, FLAGS, "algo" },
    { "algo",          "Scaling algorithm",   OFFSET(algorithm), AV_OPT_TYPE_INT,    { .i64 = F_BILINEAR      }, INT_MIN, INT_MAX, FLAGS, "algo" },
        { "area",          "area averaging",      0,             AV_OPT_TYPE_CONST,  { .i64 = F_AREA          }, 0, 0, FLAGS, "algo" },
        { "bicubic",       "bicubic",             0,             AV_OPT_TYPE_CONST,  { .i64 = F_BICUBIC       }, 0, 0, FLAGS, "algo" },
        { "bilinear",      "bilinear",            0,             AV_OPT_TYPE_CONST,  { .i64 = F_BILINEAR      }, 0, 0, FLAGS, "algo" },
        { "gauss",         "Gaussian",            0,             AV_OPT_TYPE_CONST,  { .i64 = F_GAUSSIAN      }, 0, 0, FLAGS, "algo" },
        { "lanczos",       "Lanczos",             0,             AV_OPT_TYPE_CONST,  { .i64 = F_LANCZOS       }, 0, 0, FLAGS, "algo" },
        { "neighbor",      "nearest neighbor",    0,             AV_OPT_TYPE_CONST,  { .i64 = F_NEIGHBOR      }, 0, 0, FLAGS, "algo" },
        { "sinc",          "sinc",                0,             AV_OPT_TYPE_CONST,  { .i64 = F_SINC          }, 0, 0, FLAGS, "algo" },
        { "spline",        "bicubic spline",      0,             AV_OPT_TYPE_CONST,  { .i64 = F_SPLINE        }, 0, 0, FLAGS, "algo" },
        { "experimental",  "experimental",        0,             AV_OPT_TYPE_CONST,  { .i64 = F_EXPERIMENTAL  }, 0, 0, FLAGS, "algo" },
    { "force_original_aspect_ratio", "decrease or increase w/h if necessary to keep the original AR", OFFSET(force_original_aspect_ratio), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 2, FLAGS, "force_oar" },
        { "disable",       NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 0 }, 0, 0, FLAGS, "force_oar" },
        { "decrease",      NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 1 }, 0, 0, FLAGS, "force_oar" },
        { "increase",      NULL, 0, AV_OPT_TYPE_CONST, {.i64 = 2 }, 0, 0, FLAGS, "force_oar" },
    { "force_divisible_by", "enforce that the output resolution is divisible by a defined integer when force_original_aspect_ratio is used", OFFSET(force_divisible_by), AV_OPT_TYPE_INT, { .i64 = 1 }, 1, 256, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(scale_opencl);

static const AVFilterPad scale_opencl_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = &scale_opencl_filter_frame,
        .config_props = &config_input_props,
    },
    { NULL }
};

static const AVFilterPad scale_opencl_outputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = &config_output_props,
    },
    { NULL }
};

AVFilter ff_vf_scale_opencl = {
    .name           = "scale_opencl",
    .description    = NULL_IF_CONFIG_SMALL("Scale the input video size."),
    .priv_size      = sizeof(ScaleOpenCLContext),
    .priv_class     = &scale_opencl_class,
    .init           = &ff_opencl_filter_init,
    .uninit         = &scale_opencl_uninit,
    .query_formats  = &ff_opencl_filter_query_formats,
    .inputs         = scale_opencl_inputs,
    .outputs        = scale_opencl_outputs,
    .flags_internal = FF_FILTER_FLAG_HWFRAME_AWARE,
};
