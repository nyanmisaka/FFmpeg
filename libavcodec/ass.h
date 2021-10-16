/*
 * SSA/ASS common functions
 * Copyright (c) 2010  Aurelien Jacobs <aurel@gnuage.org>
 * Copyright (c) 2021 The FFmpeg Project
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

#ifndef AVCODEC_ASS_H
#define AVCODEC_ASS_H

#include "avcodec.h"
#include "libavutil/ass_internal.h"

typedef struct FFASSDecoderContext {
    int readorder;
} FFASSDecoderContext;

static inline int ff_ass_subtitle_header_full(AVCodecContext *avctx,
                                int play_res_x, int play_res_y,
                                const char *font, int font_size,
                                int primary_color, int secondary_color,
                                int outline_color, int back_color,
                                int bold, int italic, int underline,
                                int border_style, int alignment)
{
    avctx->subtitle_header = (uint8_t *)avpriv_ass_get_subtitle_header_full(
                                play_res_x, play_res_y, font, font_size,
                                primary_color, secondary_color, outline_color, 
                                back_color, bold,italic,underline,border_style,alignment,
                                !(avctx->flags & AV_CODEC_FLAG_BITEXACT));

    if (!avctx->subtitle_header)
        return AVERROR(ENOMEM);
    avctx->subtitle_header_size = strlen((char *)avctx->subtitle_header);
    return 0;
}

static inline int ff_ass_subtitle_header_default(AVCodecContext *avctx)
{
    avctx->subtitle_header = (uint8_t *)avpriv_ass_get_subtitle_header_default(!(avctx->flags & AV_CODEC_FLAG_BITEXACT));

    if (!avctx->subtitle_header)
        return AVERROR(ENOMEM);
    avctx->subtitle_header_size = strlen((char *)avctx->subtitle_header);
    return 0;
}

static inline void ff_ass_decoder_flush(AVCodecContext *avctx)
{
    FFASSDecoderContext *s = avctx->priv_data;
    if (!(avctx->flags2 & AV_CODEC_FLAG2_RO_FLUSH_NOOP))
        s->readorder = 0;
}

#endif /* AVCODEC_ASS_H */
