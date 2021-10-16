/*
 * Copyright (c) 2021 softworkz
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
 * overlay text subtitles on top of a video frame
 */

#include <ass/ass.h>
#include <libavutil/ass_internal.h>
#include <libavutil/thread.h>

#include "drawutils.h"
#include "filters.h"

#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

typedef struct TextSubsContext {
    const AVClass *class;
    AVMutex mutex;

    ASS_Library   *library;
    ASS_Renderer  *renderer;
    ASS_Track     *track;

    char *default_font_path;
    char *fonts_dir;
    char *fc_file;
    double font_size;
    char *force_style;
    char *language;
    int margin;
    int render_latest_only;

    int alpha;
    FFDrawContext draw;

    int got_header;
    int out_w, out_h;
    AVRational frame_rate;
    AVFrame *last_frame;
    int eof;
} TextSubsContext;

/* libass supports a log level ranging from 0 to 7 */
static const int ass_libavfilter_log_level_map[] = {
    AV_LOG_QUIET,               /* 0 */
    AV_LOG_PANIC,               /* 1 */
    AV_LOG_FATAL,               /* 2 */
    AV_LOG_ERROR,               /* 3 */
    AV_LOG_WARNING,             /* 4 */
    AV_LOG_INFO,                /* 5 */
    AV_LOG_VERBOSE,             /* 6 */
    AV_LOG_DEBUG,               /* 7 */
};

static void ass_log(int ass_level, const char *fmt, va_list args, void *ctx)
{
    const int ass_level_clip = av_clip(ass_level, 0, FF_ARRAY_ELEMS(ass_libavfilter_log_level_map) - 1);
    const int level = ass_libavfilter_log_level_map[ass_level_clip];

    av_vlog(ctx, level, fmt, args);
    av_log(ctx, level, "\n");
}

static av_cold void uninit(AVFilterContext *ctx)
{
    TextSubsContext *s = ctx->priv;

    if (s->track)
        ass_free_track(s->track);
    if (s->renderer)
        ass_renderer_done(s->renderer);
    if (s->library)
        ass_library_done(s->library);

    s->track = NULL;
    s->renderer = NULL;
    s->library = NULL;

    ff_mutex_destroy(&s->mutex);

    av_frame_free(&s->last_frame);
}

static int overlay_textsubs_query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats;
    AVFilterLink *inlink0 = ctx->inputs[0];
    AVFilterLink *inlink1 = ctx->inputs[1];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSubtitleType subtitle_fmts[] = { AV_SUBTITLE_FMT_ASS, AV_SUBTITLE_FMT_NONE };
    int ret;

    /* set input0 video formats */
    formats = ff_draw_supported_pixel_formats(0);
    if ((ret = ff_formats_ref(formats, &inlink0->outcfg.formats)) < 0)
        return ret;

    /* set input1 subtitle formats */
    formats = ff_make_format_list(subtitle_fmts);
    if ((ret = ff_formats_ref(formats, &inlink1->outcfg.formats)) < 0)
        return ret;

    /* set output0 video formats */
    formats = ff_draw_supported_pixel_formats(0);
    if ((ret = ff_formats_ref(formats, &outlink->incfg.formats)) < 0)
        return ret;

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;

    outlink->w = ctx->inputs[0]->w;
    outlink->h = ctx->inputs[0]->h;
    outlink->time_base = ctx->inputs[0]->time_base;

    return 0;
}

static int config_input_main(AVFilterLink *inlink)
{
    AVFilterContext *ctx  = inlink->dst;
    TextSubsContext *s = inlink->dst->priv;
    int ret;

    ret = ff_draw_init(&s->draw, inlink->format, s->alpha ? FF_DRAW_PROCESS_ALPHA : 0);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Could not initialize ff_draw.\n");
        return ret;
    }

    ass_set_frame_size  (s->renderer, inlink->w, inlink->h);
    ass_set_pixel_aspect(s->renderer, av_q2d(inlink->sample_aspect_ratio));

    av_log(ctx, AV_LOG_VERBOSE, "Subtitle screen: %dx%d\n\n\n\n", inlink->w, inlink->h);

    return 0;
}

/* libass stores an RGBA color in the format RRGGBBTT, where TT is the transparency level */
#define AR(c)  ( (c)>>24)
#define AG(c)  (((c)>>16)&0xFF)
#define AB(c)  (((c)>>8) &0xFF)
#define AA(c)  ((0xFF-(c)) &0xFF)

static void overlay_ass_image(TextSubsContext *s, AVFrame *picref,
                              const ASS_Image *image)
{
    for (; image; image = image->next) {
        uint8_t rgba_color[] = {AR(image->color), AG(image->color), AB(image->color), AA(image->color)};
        FFDrawColor color;
        ff_draw_color(&s->draw, &color, rgba_color);
        ff_blend_mask(&s->draw, &color,
                      picref->data, picref->linesize,
                      picref->width, picref->height,
                      image->bitmap, image->stride, image->w, image->h,
                      3, 0, image->dst_x, image->dst_y);
    }
}

static void process_header(AVFilterContext *link, AVFrame *sub)
{
    TextSubsContext *s = link->priv;
    ASS_Track *track = s->track;
    ASS_Style *style;
    int sid = 0;

    if (!track)
        return;

    if (sub && sub->subtitle_header) {
        char *subtitle_header = (char *)sub->subtitle_header->data;
        ass_process_codec_private(s->track, subtitle_header, strlen(subtitle_header));
    }
    else {
        char* subtitle_header = avpriv_ass_get_subtitle_header_default(0);
        if (!subtitle_header)
            return;

        ass_process_codec_private(s->track, subtitle_header, strlen(subtitle_header));
        av_free(subtitle_header);
    }

    if (s->language)
        s->track->Language = strdup(s->language);

    if (!s->track->event_format) {
        s->track->event_format = strdup("ReadOrder, Layer, Style, Name, MarginL, MarginR, MarginV, Effect, Text");
    }

    if (s->track->n_styles == 0) {
        sid = ass_alloc_style(track);
        style = &s->track->styles[sid];
        style->Name             = strdup("Default");
        style->PrimaryColour    = 0xffffff00;
        style->SecondaryColour  = 0x00ffff00;
        style->OutlineColour    = 0x00000000;
        style->BackColour       = 0x00000080;
        style->Bold             = 200;
        style->ScaleX           = 1.0;
        style->ScaleY           = 1.0;
        style->Spacing          = 0;
        style->BorderStyle      = 1;
        style->Outline          = 2;
        style->Shadow           = 3;
        style->Alignment        = 2;
    }
    else
        style = &s->track->styles[sid];

    style->FontSize         = s->font_size;
    style->MarginL = style->MarginR = style->MarginV = s->margin;

    track->default_style = sid;

    s->got_header = 1;
}

static int filter_video_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    TextSubsContext *s = ctx->priv;
    int detect_change = 0;
    ASS_Image *image;

    int64_t time_ms = frame->pts * av_q2d(inlink->time_base) * 1000;

    ff_mutex_lock(&s->mutex);
    image = ass_render_frame(s->renderer, s->track, time_ms, &detect_change);
    ff_mutex_unlock(&s->mutex);

    if (detect_change)
        av_log(ctx, AV_LOG_DEBUG, "Change happened at time ms:%"PRId64"\n", time_ms);

    overlay_ass_image(s, frame, image);

    return ff_filter_frame(ctx->outputs[0], frame);
}

static int filter_subtitle_frame(AVFilterLink *inlink, AVFrame *sub)
{
    AVFilterContext *ctx = inlink->dst;
    TextSubsContext *s = ctx->priv;
    const int64_t start_time = av_rescale_q(sub->subtitle_pts, AV_TIME_BASE_Q, av_make_q(1, 1000));
    const int64_t duration   = sub->subtitle_end_time;

    if (!s->got_header)
        process_header(ctx, sub);

    for (unsigned i = 0; i < sub->num_subtitle_areas; i++) {
        char *ass_line = sub->subtitle_areas[i]->ass;
        if (!ass_line)
            break;

        ff_mutex_lock(&s->mutex);
        ass_process_chunk(s->track, ass_line, strlen(ass_line), start_time, duration);

        if (s->render_latest_only && s->track->n_events > 1) {
            const int64_t diff = s->track->events[s->track->n_events - 1].Start
                               - s->track->events[s->track->n_events - 2].Start;
            if (s->track->events[s->track->n_events - 2].Duration > diff)
                s->track->events[s->track->n_events - 2].Duration = diff;
        }

        ff_mutex_unlock(&s->mutex);
    }

    av_frame_free(&sub);
    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    TextSubsContext *s = ctx->priv;

    s->library = ass_library_init();

    if (!s->library) {
        av_log(ctx, AV_LOG_ERROR, "Could not initialize libass.\n");
        return AVERROR(EINVAL);
    }

    ass_set_message_cb(s->library, ass_log, ctx);

    /* Initialize fonts */
    if (s->fonts_dir)
        ass_set_fonts_dir(s->library, s->fonts_dir);

    ass_set_extract_fonts(s->library, 1);

    s->renderer = ass_renderer_init(s->library);
    if (!s->renderer) {
        av_log(ctx, AV_LOG_ERROR, "Could not initialize libass renderer.\n");
        return AVERROR(EINVAL);
    }

    s->track = ass_new_track(s->library);
    if (!s->track) {
        av_log(ctx, AV_LOG_ERROR, "ass_new_track() failed!\n");
        return AVERROR(EINVAL);
    }

    ass_set_fonts(s->renderer, s->default_font_path, NULL, 1, s->fc_file, 1);

    if (s->force_style) {
        char **list = NULL;
        char *temp = NULL;
        char *ptr = av_strtok(s->force_style, ",", &temp);
        int i = 0;
        while (ptr) {
            av_dynarray_add(&list, &i, ptr);
            if (!list) {
                return AVERROR(ENOMEM);
            }
            ptr = av_strtok(NULL, ",", &temp);
        }
        av_dynarray_add(&list, &i, NULL);
        if (!list) {
            return AVERROR(ENOMEM);
        }
        ass_set_style_overrides(s->library, list);
        av_free(list);
    }

    ff_mutex_init(&s->mutex, NULL);

    return 0;
}

static int textsub2video_query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats;
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSubtitleType subtitle_fmts[] = { AV_SUBTITLE_FMT_ASS, AV_SUBTITLE_FMT_NONE };
    int ret;

    /* set input0 subtitle format */
    formats = ff_make_format_list(subtitle_fmts);
    if ((ret = ff_formats_ref(formats, &inlink->outcfg.formats)) < 0)
        return ret;

    /* set output0 video format */
    formats = ff_draw_supported_pixel_formats(AV_PIX_FMT_FLAG_ALPHA);
    if ((ret = ff_formats_ref(formats, &outlink->incfg.formats)) < 0)
        return ret;

    return 0;
}

static int textsub2video_config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    TextSubsContext *s = ctx->priv;

    if (s->out_w <= 0 || s->out_h <= 0) {
        s->out_w = inlink->w;
        s->out_h = inlink->h;
    }

    return 0;
}

static int textsub2video_config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    TextSubsContext *s = ctx->priv;
    int ret;

    ret = ff_draw_init(&s->draw, outlink->format, FF_DRAW_PROCESS_ALPHA);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Could not initialize ff_draw.\n");
        return ret;
    }

    if (s->out_w <= 0 || s->out_h <= 0) {
        av_log(ctx, AV_LOG_ERROR, "No output image size set.\n");
        return AVERROR(EINVAL);
    }

    ass_set_frame_size  (s->renderer, s->out_w, s->out_h);

    outlink->w = s->out_w;
    outlink->h = s->out_h;
    outlink->sample_aspect_ratio = (AVRational){1,1};
    outlink->frame_rate = s->frame_rate;

    return 0;
}

static int textsub2video_filter_frame(AVFilterLink *inlink, AVFrame *sub)
{
    AVFilterContext *ctx = inlink->dst;
    TextSubsContext *s = ctx->priv;
    const int64_t start_time = av_rescale_q(sub->subtitle_pts, AV_TIME_BASE_Q, av_make_q(1, 1000));
    const int64_t duration   = sub->subtitle_end_time;

    av_log(ctx, AV_LOG_VERBOSE, "textsub2video_filter_frame num_subtitle_rects: %d\n", sub->num_subtitle_areas);

    if (!s->got_header)
        process_header(ctx, sub);

    for (unsigned i = 0; i < sub->num_subtitle_areas; i++) {
        char *ass_line = sub->subtitle_areas[i]->ass;
        if (!ass_line)
            break;
        ff_mutex_lock(&s->mutex);
        ass_process_chunk(s->track, ass_line, strlen(ass_line), start_time, duration);
        ff_mutex_unlock(&s->mutex);
    }

    av_frame_free(&sub);
    return 0;
}

static int textsub2video_request_frame(AVFilterLink *outlink)
{
    TextSubsContext *s = outlink->src->priv;
    const AVFilterLink *inlink = outlink->src->inputs[0];
    int64_t last_pts = outlink->current_pts;
    int64_t next_pts, time_ms;
    int i, detect_change = 0;
    AVFrame *out;
    ASS_Image *image;

    if (last_pts == AV_NOPTS_VALUE)
        last_pts = inlink->current_pts * av_q2d(inlink->time_base) / av_q2d(outlink->time_base);

    next_pts = last_pts + (1.0 / av_q2d(outlink->frame_rate) / av_q2d(outlink->time_base));

    time_ms = next_pts * av_q2d(outlink->time_base) * 1000;

    image = ass_render_frame(s->renderer, s->track, time_ms, &detect_change);

    if (detect_change)
        av_log(outlink->src, AV_LOG_DEBUG, "Change happened at time ms:%"PRId64"\n", time_ms);
    else if (s->last_frame) {
        out = av_frame_clone(s->last_frame);
        if (!out)
            return AVERROR(ENOMEM);

        out->pts = next_pts;
        return ff_filter_frame(outlink, out);
    }

    out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    if (!out)
        return AVERROR(ENOMEM);

    for (i = 2; i < AV_NUM_DATA_POINTERS; i++) {
        if (out->buf[i] && i != 1)
            memset(out->buf[i]->data, 0, out->buf[i]->size);
    }

    out->pts = next_pts;

    if (image)
        overlay_ass_image(s, out, image);

    av_frame_free(&s->last_frame);

    s->last_frame = av_frame_clone(out);

    return ff_filter_frame(outlink, out);
}

static int activate(AVFilterContext *ctx)
{
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    TextSubsContext *s = ctx->priv;
    AVFrame *in;
    int ret;

    FF_FILTER_FORWARD_STATUS_BACK(outlink, inlink);

    ret = ff_inlink_consume_frame(inlink, &in);
    if (ret < 0)
        return ret;
    if (ret > 0)
        return textsub2video_filter_frame(inlink, in);

    if (ff_outlink_frame_wanted(outlink)) {
        if (!s->eof && ff_outlink_get_status(ctx->inputs[0])) {
            s->eof = 1;
        }
        if (!s->eof && ff_inlink_queued_frames(ctx->inputs[0]) == 0)
            ff_inlink_request_frame(ctx->inputs[0]);
        if (s->eof && ff_inlink_queued_frames(ctx->inputs[0]) <= 0) {
            ff_outlink_set_status(outlink, AVERROR_EOF, AV_NOPTS_VALUE);
        }
        else {
            return textsub2video_request_frame(outlink);
        }

        return 0;
    }

    return FFERROR_NOT_READY;
}

#define OFFSET(x) offsetof(TextSubsContext, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_FILTERING_PARAM)

static const AVOption overlaytextsubs_options[] = {
    {"alpha",              "enable processing of alpha channel", OFFSET(alpha),              AV_OPT_TYPE_BOOL,   {.i64 = 0   }, 0,         1,        FLAGS},
    {"font_size",          "default font size",                  OFFSET(font_size),          AV_OPT_TYPE_DOUBLE, {.dbl = 18.0}, 0.0,       100.0,    FLAGS},
    {"force_style",        "force subtitle style",               OFFSET(force_style),        AV_OPT_TYPE_STRING, {.str = NULL}, 0,         0,        FLAGS},
    {"margin",             "default margin",                     OFFSET(margin),             AV_OPT_TYPE_INT,    {.i64 = 20  }, 0,         INT_MAX,  FLAGS},
    {"default_font_path",  "path to default font",               OFFSET(default_font_path),  AV_OPT_TYPE_STRING, {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fonts_dir",          "directory to scan for fonts",        OFFSET(fonts_dir),          AV_OPT_TYPE_STRING, {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fontsdir",           "directory to scan for fonts",        OFFSET(fonts_dir),          AV_OPT_TYPE_STRING, {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fontconfig_file",    "fontconfig file to load",            OFFSET(fc_file),            AV_OPT_TYPE_STRING, {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"language",           "default language",                   OFFSET(language),           AV_OPT_TYPE_STRING, {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"render_latest_only", "newest sub event for each time",     OFFSET(render_latest_only), AV_OPT_TYPE_BOOL,   {.i64 = 0   }, 0,         1,        FLAGS},
    { NULL }
};

static const AVOption textsub2video_options[] = {
    {"rate",               "set frame rate",                   OFFSET(frame_rate),         AV_OPT_TYPE_VIDEO_RATE, {.str="8"},   0,         INT_MAX,  FLAGS},
    {"r",                  "set frame rate",                   OFFSET(frame_rate),         AV_OPT_TYPE_VIDEO_RATE, {.str="8"},   0,         INT_MAX,  FLAGS},
    {"size",               "set video size",                   OFFSET(out_w),              AV_OPT_TYPE_IMAGE_SIZE, {.str = NULL}, 0,         0,        FLAGS},
    {"s",                  "set video size",                   OFFSET(out_w),              AV_OPT_TYPE_IMAGE_SIZE, {.str = NULL}, 0,         0,        FLAGS},
    {"font_size",          "default font size",                OFFSET(font_size),          AV_OPT_TYPE_DOUBLE,     {.dbl = 18.0}, 0.0,       100.0,    FLAGS},
    {"force_style",        "force subtitle style",             OFFSET(force_style),        AV_OPT_TYPE_STRING,     {.str = NULL}, 0,         0,        FLAGS},
    {"margin",             "default margin",                   OFFSET(margin),             AV_OPT_TYPE_INT,        {.i64 = 20  }, 0,         INT_MAX,  FLAGS},
    {"default_font_path",  "path to default font",             OFFSET(default_font_path),  AV_OPT_TYPE_STRING,     {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fonts_dir",          "directory to scan for fonts",      OFFSET(fonts_dir),          AV_OPT_TYPE_STRING,     {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fontsdir",           "directory to scan for fonts",      OFFSET(fonts_dir),          AV_OPT_TYPE_STRING,     {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"fontconfig_file",    "fontconfig file to load",          OFFSET(fc_file),            AV_OPT_TYPE_STRING,     {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"language",           "default language",                 OFFSET(language),           AV_OPT_TYPE_STRING,     {.str = NULL}, CHAR_MIN,  CHAR_MAX, FLAGS},
    {"render_latest_only", "newest sub event for each time",   OFFSET(render_latest_only), AV_OPT_TYPE_BOOL,       {.i64 = 0   }, 0,         1,        FLAGS},
    { NULL }
};

#if CONFIG_OVERLAYTEXTSUBS_FILTER

AVFILTER_DEFINE_CLASS(overlaytextsubs);

static const AVFilterPad overlaytextsubs_inputs[] = {
    {
        .name         = "main",
        .type         = AVMEDIA_TYPE_VIDEO,
        .config_props = config_input_main,
        .filter_frame = filter_video_frame,
    },
    {
        .name         = "overlay",
        .type         = AVMEDIA_TYPE_SUBTITLE,
        .filter_frame = filter_subtitle_frame,
    },
    { NULL }
};

static const AVFilterPad overlaytextsubs_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = config_output,
    },
    { NULL }
};

const AVFilter ff_vf_overlaytextsubs = {
    .name          = "overlaytextsubs",
    .description   = NULL_IF_CONFIG_SMALL("Overlay textual subtitles on top of the input."),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(TextSubsContext),
    .priv_class    = &overlaytextsubs_class,
    .inputs        = overlaytextsubs_inputs,
    .outputs       = overlaytextsubs_outputs,
    .query_formats = overlay_textsubs_query_formats,
};
#endif

#if CONFIG_TEXTSUB2VIDEO_FILTER

AVFILTER_DEFINE_CLASS(textsub2video);

static const AVFilterPad textsub2video_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_SUBTITLE,
        .config_props = textsub2video_config_input,
    },
    { NULL }
};

static const AVFilterPad textsub2video_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_VIDEO,
        .config_props  = textsub2video_config_output,
    },
    { NULL }
};

const AVFilter ff_svf_textsub2video = {
    .name          = "textsub2video",
    .description   = NULL_IF_CONFIG_SMALL("Convert textual subtitles to video frames"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(TextSubsContext),
    .priv_class    = &textsub2video_class,
    .activate      = activate,
    .inputs        = textsub2video_inputs,
    .outputs       = textsub2video_outputs,
    .query_formats = textsub2video_query_formats,
};
#endif
