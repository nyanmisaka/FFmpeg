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
 * subtitle filter to convert graphical subs to text subs via OCR
 */

#include <tesseract/capi.h>
#include <libavutil/ass_internal.h>

#include "libavutil/opt.h"
#include "subtitles.h"

typedef struct SubOcrContext {
    const AVClass *class;
    int w, h;

    TessBaseAPI *tapi;
    TessOcrEngineMode ocr_mode;
    char *tessdata_path;
    char *language;

    int readorder_counter;

    AVFrame *pending_frame;
} SubOcrContext;


static int init(AVFilterContext *ctx)
{
    SubOcrContext *s = ctx->priv;
    const char* tver = TessVersion();
    int ret;

    s->tapi = TessBaseAPICreate();

    if (!s->tapi || !tver || !strlen(tver)) {
        av_log(ctx, AV_LOG_ERROR, "Failed to access libtesseract\n");
        return AVERROR(ENOSYS);
    }

    av_log(ctx, AV_LOG_VERBOSE, "Initializing libtesseract, version: %s\n", tver);

    ret = TessBaseAPIInit4(s->tapi, s->tessdata_path, s->language, s->ocr_mode, NULL, 0, NULL, NULL, 0, 1);
    if (ret < 0 ) {
        av_log(ctx, AV_LOG_ERROR, "Failed to initialize libtesseract. Error: %d\n", ret);
        return AVERROR(ENOSYS);
    }

    ret = TessBaseAPISetVariable(s->tapi, "tessedit_char_blacklist", "|");
    if (ret < 0 ) {
        av_log(ctx, AV_LOG_ERROR, "Failed to set 'tessedit_char_blacklist'. Error: %d\n", ret);
        return AVERROR(EINVAL);
    }

    return 0;
}

static void uninit(AVFilterContext *ctx)
{
    SubOcrContext *s = ctx->priv;

    if (s->tapi) {
        TessBaseAPIEnd(s->tapi);
        TessBaseAPIDelete(s->tapi);
    }
}

static int query_formats(AVFilterContext *ctx)
{
    AVFilterFormats *formats, *formats2;
    AVFilterLink *inlink = ctx->inputs[0];
    AVFilterLink *outlink = ctx->outputs[0];
    static const enum AVSubtitleType in_fmts[] = { AV_SUBTITLE_FMT_BITMAP, AV_SUBTITLE_FMT_NONE };
    static const enum AVSubtitleType out_fmts[] = { AV_SUBTITLE_FMT_ASS, AV_SUBTITLE_FMT_NONE };
    int ret;

    /* set input format */
    formats = ff_make_format_list(in_fmts);
    if ((ret = ff_formats_ref(formats, &inlink->outcfg.formats)) < 0)
        return ret;

    /* set output format */
    formats2 = ff_make_format_list(out_fmts);
    if ((ret = ff_formats_ref(formats2, &outlink->incfg.formats)) < 0)
        return ret;

    return 0;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    SubOcrContext *s = ctx->priv;

    if (s->w <= 0 || s->h <= 0) {
        s->w = inlink->w;
        s->h = inlink->h;
    }
    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    const AVFilterContext *ctx  = outlink->src;
    SubOcrContext *s = ctx->priv;

    outlink->format = AV_SUBTITLE_FMT_ASS;
    outlink->w = s->w;
    outlink->h = s->h;

    return 0;
}

static uint8_t* create_grayscale_image(AVFilterContext *ctx, AVSubtitleArea *area)
{
    uint8_t gray_pal[256];
    const size_t img_size = area->buf[0]->size;
    const uint8_t* img    = area->buf[0]->data;
    uint8_t* gs_img       = av_malloc(img_size);

    if (!gs_img)
        return NULL;

    for (unsigned i = 0; i < 256; i++) {
        const uint8_t *col = (uint8_t*)&area->pal[i];
        const int val      = (int)col[3] * FFMAX3(col[0], col[1], col[2]);
        gray_pal[i]        = (uint8_t)(val >> 8);
    }

    for (unsigned i = 0; i < img_size; i++)
        gs_img[i] = 255 - gray_pal[img[i]];

    return gs_img;
}

static int convert_area(AVFilterContext *ctx, AVSubtitleArea *area)
{
    SubOcrContext *s = ctx->priv;
    char *ocr_text = NULL;
    int ret;
    uint8_t *gs_img = create_grayscale_image(ctx, area);

    if (!gs_img)
        return AVERROR(ENOMEM);

    area->type = AV_SUBTITLE_FMT_ASS;
    TessBaseAPISetImage(s->tapi, gs_img, area->w, area->h, 1, area->linesize[0]);
    TessBaseAPISetSourceResolution(s->tapi, 70);

    ret = TessBaseAPIRecognize(s->tapi, NULL);
    if (ret == 0)
        ocr_text = TessBaseAPIGetUTF8Text(s->tapi);

    if (!ocr_text) {
        av_log(ctx, AV_LOG_WARNING, "OCR didn't return a text. ret=%d\n", ret);
        area->ass = NULL;
    }
    else {
        const size_t len = strlen(ocr_text);

        if (len > 0 && ocr_text[len - 1] == '\n')
            ocr_text[len - 1] = 0;

        av_log(ctx, AV_LOG_VERBOSE, "OCR Result: %s\n", ocr_text);

        area->ass = av_strdup(ocr_text);

        TessDeleteText(ocr_text);
    }

    av_freep(&gs_img);
    av_buffer_unref(&area->buf[0]);
    area->type = AV_SUBTITLE_FMT_ASS;

    return 0;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    SubOcrContext *s = ctx->priv;
    AVFilterLink *outlink = inlink->dst->outputs[0];
    int ret, frame_sent = 0;

    if (s->pending_frame) {
        const uint64_t pts_diff = frame->subtitle_pts - s->pending_frame->subtitle_pts;
        s->pending_frame->subtitle_end_time = (uint32_t)(pts_diff / 1000);

        ret = ff_filter_frame(outlink, s->pending_frame);
        s->pending_frame = NULL;
        if (ret < 0)
            return  ret;

        frame_sent = 1;

        if (frame->num_subtitle_areas == 0) {
            // No need to forward this empty frame
            av_frame_free(&frame);
            return 0;
        }
    }

    ret = av_frame_make_writable(frame);

    if (ret < 0) {
        av_frame_free(&frame);
        return ret;
    }

    frame->format = AV_SUBTITLE_FMT_ASS;

    av_log(ctx, AV_LOG_DEBUG, "filter_frame sub_pts: %"PRIu64", start_time: %d, end_time: %d, num_areas: %d\n", 
        frame->subtitle_pts, frame->subtitle_start_time, frame->subtitle_end_time, frame->num_subtitle_areas);

    if (frame->num_subtitle_areas > 1 &&
        frame->subtitle_areas[0]->y > frame->subtitle_areas[frame->num_subtitle_areas - 1]->y) {

        for (unsigned i = 0; i < frame->num_subtitle_areas / 2; i++)
            FFSWAP(AVSubtitleArea*, frame->subtitle_areas[i], frame->subtitle_areas[frame->num_subtitle_areas - i - 1]);
    }
    
    for (unsigned i = 0; i < frame->num_subtitle_areas; i++) {
        AVSubtitleArea *area = frame->subtitle_areas[i];

        ret = convert_area(ctx, area);
        if (ret < 0)
            return ret;

        if (area->ass && area->ass[0] != '\0') {
            char *tmp = area->ass;

            if (i == 0)
                area->ass = avpriv_ass_get_dialog(s->readorder_counter++, 0, "Default", NULL, tmp);
            else
                area->ass = av_asprintf("\\N%s", tmp);

            av_free(tmp);
        }
    }

    // When decoders can't determine the end time, they are setting it either to UINT32_NAX
    // or 30s (dvbsub).
    if (frame->num_subtitle_areas > 0 && frame->subtitle_end_time >= 30000) {
        // Can't send it without end time, wait for the next frame to determine the end_display time
        s->pending_frame = frame;

        if (frame_sent)
            return 0;

        // To keep all going, send an empty frame instead
        frame = ff_get_subtitles_buffer(outlink, AV_SUBTITLE_FMT_ASS);
        if (!frame)
            return AVERROR(ENOMEM);

        av_frame_copy_props(frame, s->pending_frame);
        frame->subtitle_end_time = 1;
    }

    return ff_filter_frame(outlink, frame);
}

#define OFFSET(x) offsetof(SubOcrContext, x)
#define FLAGS (AV_OPT_FLAG_SUBTITLE_PARAM | AV_OPT_FLAG_FILTERING_PARAM)

static const AVOption graphicsub2text_options[] = {
    { "ocr_mode",       "set ocr mode",                  OFFSET(ocr_mode),      AV_OPT_TYPE_INT,    {.i64=OEM_TESSERACT_ONLY},          OEM_TESSERACT_ONLY, 2, FLAGS, "ocr_mode" },
    {   "tesseract",    "classic tesseract ocr",         0,                     AV_OPT_TYPE_CONST,  {.i64=OEM_TESSERACT_ONLY},          0,                  0, FLAGS, "ocr_mode" },
    {   "lstm",         "lstm (ML based)",               0,                     AV_OPT_TYPE_CONST,  {.i64=OEM_LSTM_ONLY},               0,                  0, FLAGS, "ocr_mode" },
    {   "both",         "use both models combined",      0,                     AV_OPT_TYPE_CONST,  {.i64=OEM_TESSERACT_LSTM_COMBINED}, 0,                  0, FLAGS, "ocr_mode" },
    { "tessdata_path",  "path to tesseract data",        OFFSET(tessdata_path), AV_OPT_TYPE_STRING, {.str = NULL},                      0,                  0, FLAGS, NULL   },
    { "language",       "ocr language",                  OFFSET(language),      AV_OPT_TYPE_STRING, {.str = "eng"},                     0,                  0, FLAGS, NULL   },
    { NULL },
};

AVFILTER_DEFINE_CLASS(graphicsub2text);

static const AVFilterPad inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_SUBTITLE,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
};

static const AVFilterPad outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_SUBTITLE,
        .config_props  = config_output,
    },
};

const AVFilter ff_sf_graphicsub2text = {
    .name          = "graphicsub2text",
    .description   = NULL_IF_CONFIG_SMALL("Convert graphical subtitles to text subtitles via OCR"),
    .init          = init,
    .uninit        = uninit,
    .priv_size     = sizeof(SubOcrContext),
    .priv_class    = &graphicsub2text_class,
    FILTER_INPUTS(inputs),
    FILTER_OUTPUTS(outputs),
    FILTER_QUERY_FUNC(query_formats),
};
