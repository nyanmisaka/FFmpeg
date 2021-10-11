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

#include <string.h>
#include "common.h"
#include "subfmt.h"

typedef struct SubtitleFmtInfo {
    enum AVSubtitleType fmt;
    const char* name;
} SubtitleFmtInfo;

static SubtitleFmtInfo sub_fmt_info[AV_SUBTITLE_FMT_NB] = {
    {.fmt = AV_SUBTITLE_FMT_UNKNOWN, .name = "Unknown subtitle format", },
    {.fmt = AV_SUBTITLE_FMT_BITMAP,  .name = "Graphical subtitles",     },
    {.fmt = AV_SUBTITLE_FMT_TEXT,    .name = "Text subtitles (plain)",  },
    {.fmt = AV_SUBTITLE_FMT_ASS,     .name = "Text subtitles (ass)",    },
};

const char *av_get_subtitle_fmt_name(enum AVSubtitleType sub_fmt)
{
    if (sub_fmt < 0 || sub_fmt >= AV_SUBTITLE_FMT_NB)
        return NULL;
    return sub_fmt_info[sub_fmt].name;
}

enum AVSubtitleType av_get_subtitle_fmt(const char *name)
{
    for (int i = 0; i < AV_SUBTITLE_FMT_NB; i++)
        if (!strcmp(sub_fmt_info[i].name, name))
            return i;
    return AV_SUBTITLE_FMT_NONE;
}

int av_subtitle_area2rect(AVSubtitleRect *dst, const AVSubtitleArea *src)
{
    dst->x         =  src->x;        
    dst->y         =  src->y;        
    dst->w         =  src->w;        
    dst->h         =  src->h;        
    dst->nb_colors =  src->nb_colors;
    dst->type      =  src->type;                     

    switch (dst->type) {
    case AV_SUBTITLE_FMT_BITMAP:

        if (src->h > 0 && src->w > 0 && src->buf[0]) {
            uint32_t *pal;
            AVBufferRef *buf = src->buf[0];
            dst->data[0] = av_mallocz(buf->size);
            memcpy(dst->data[0], buf->data, buf->size);
            dst->linesize[0] = src->linesize[0];

            dst->data[1] = av_mallocz(256 * 4);
            pal = (uint32_t *)dst->data[1];

            for (unsigned i = 0; i < 256; i++) {
                pal[i] = src->pal[i];
            }
        }

        break;
    case AV_SUBTITLE_FMT_TEXT:

        if (src->text)
            dst->text = av_strdup(src->text);
        else
            dst->text = av_strdup("");

        if (!dst->text)
            return AVERROR(ENOMEM);

        break;
    case AV_SUBTITLE_FMT_ASS:

        if (src->ass)
            dst->ass = av_strdup(src->ass);
        else
            dst->ass = av_strdup("");

        if (!dst->ass)
            return AVERROR(ENOMEM);

        break;
    default:

        av_log(NULL, AV_LOG_ERROR, "Subtitle rect has invalid format: %d", dst->type);
        return AVERROR(ENOMEM);
    }

    return 0;
}

int av_subtitle_rect2area(AVSubtitleArea *dst, const AVSubtitleRect *src)
{
    dst->x         =  src->x;        
    dst->y         =  src->y;        
    dst->w         =  src->w;        
    dst->h         =  src->h;        
    dst->nb_colors =  src->nb_colors;
    dst->type      =  src->type;                     

    switch (dst->type) {
    case AV_SUBTITLE_FMT_BITMAP:

        if (src->h > 0 && src->w > 0 && src->data[0]) {
            AVBufferRef *buf = av_buffer_allocz(src->h * src->linesize[0]);
            memcpy(buf->data, src->data[0], buf->size);

            dst->buf[0] = buf;
            dst->linesize[0] = src->linesize[0];
        }

        if (src->data[1]) {
            uint32_t *pal = (uint32_t *)src->data[1];

            for (unsigned i = 0; i < 256; i++) {
                dst->pal[i] = pal[i];
            }
        }

        break;
    case AV_SUBTITLE_FMT_TEXT:

        if (src->text) {
            dst->text = av_strdup(src->text);
            if (!dst->text)
                return AVERROR(ENOMEM);
        }

        break;
    case AV_SUBTITLE_FMT_ASS:

        if (src->ass) {
            dst->ass = av_strdup(src->ass);
            if (!dst->ass)
                return AVERROR(ENOMEM);
        }

        break;
    default:

        av_log(NULL, AV_LOG_ERROR, "Subtitle area has invalid format: %d", dst->type);
        return AVERROR(ENOMEM);
    }

    return 0;
}

int av_subtitle_area2area(AVSubtitleArea *dst, const AVSubtitleArea *src, int copy_data)
{
    dst->x         =  src->x;        
    dst->y         =  src->y;        
    dst->w         =  src->w;        
    dst->h         =  src->h;        
    dst->nb_colors =  src->nb_colors;
    dst->type      =  src->type;                     

    switch (dst->type) {
    case AV_SUBTITLE_FMT_BITMAP:

        if (src->h > 0 && src->w > 0 && src->buf[0]) {
            dst->buf[0] = av_buffer_ref(src->buf[0]);
            if (!dst->buf[0])
                return AVERROR(ENOMEM);

            if (copy_data) {
                const int ret = av_buffer_make_writable(&dst->buf[0]);
                if (ret < 0)
                    return ret;
            }

            dst->linesize[0] = src->linesize[0];
        }

        for (unsigned i = 0; i < 256; i++)
            dst->pal[i] = src->pal[i];

        break;
    case AV_SUBTITLE_FMT_TEXT:

        if (src->text) {
            dst->text = av_strdup(src->text);
            if (!dst->text)
                return AVERROR(ENOMEM);
        }

        break;
    case AV_SUBTITLE_FMT_ASS:

        if (src->ass) {
            dst->ass = av_strdup(src->ass);
            if (!dst->ass)
                return AVERROR(ENOMEM);
        }

        break;
    default:

        av_log(NULL, AV_LOG_ERROR, "Subtitle area has invalid format: %d", dst->type);
        return AVERROR(ENOMEM);
    }

    return 0;
}

void avsubtitle_free(AVSubtitle *sub)
{
    for (unsigned i = 0; i < sub->num_rects; i++) {
        av_freep(&sub->rects[i]->data[0]);
        av_freep(&sub->rects[i]->data[1]);
        av_freep(&sub->rects[i]->data[2]);
        av_freep(&sub->rects[i]->data[3]);
        av_freep(&sub->rects[i]->text);
        av_freep(&sub->rects[i]->ass);
        av_freep(&sub->rects[i]);
    }

    av_freep(&sub->rects);

    memset(sub, 0, sizeof(*sub));
}

