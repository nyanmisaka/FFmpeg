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

#ifndef AVUTIL_SUBFMT_H
#define AVUTIL_SUBFMT_H

#include <inttypes.h>

#include "buffer.h"

enum AVSubtitleType {

    /**
     * Subtitle format unknown.
     */
    AV_SUBTITLE_FMT_NONE = -1,

    /**
     * Subtitle format unknown.
     */
    AV_SUBTITLE_FMT_UNKNOWN = 0,

    /**
     * Bitmap area in AVSubtitleRect.data, pixfmt AV_PIX_FMT_PAL8.
     */
    AV_SUBTITLE_FMT_BITMAP = 1,
    SUBTITLE_BITMAP = 1,        ///< Deprecated, use AV_SUBTITLE_FMT_BITMAP instead.

    /**
     * Plain text in AVSubtitleRect.text.
     */
    AV_SUBTITLE_FMT_TEXT = 2,
    SUBTITLE_TEXT = 2,          ///< Deprecated, use AV_SUBTITLE_FMT_TEXT instead.

    /**
     * Text Formatted as per ASS specification, contained AVSubtitleRect.ass.
     */
    AV_SUBTITLE_FMT_ASS = 3,
    SUBTITLE_ASS = 3,           ///< Deprecated, use AV_SUBTITLE_FMT_ASS instead.

    AV_SUBTITLE_FMT_NB,
};

typedef struct AVSubtitleArea {
#define AV_NUM_BUFFER_POINTERS 1

    enum AVSubtitleType type;
    int flags;

    int x;         ///< top left corner  of area.
    int y;         ///< top left corner  of area.
    int w;         ///< width            of area.
    int h;         ///< height           of area.
    int nb_colors; ///< number of colors in bitmap palette (@ref pal).

    /**
     * Buffers and line sizes for the bitmap of this subtitle.
     * 
     * @{
     */
    AVBufferRef *buf[AV_NUM_BUFFER_POINTERS];
    int linesize[AV_NUM_BUFFER_POINTERS];
    /**
     * @}
     */

    uint32_t pal[256]; ///< RGBA palette for the bitmap.

    char *text;        ///< 0-terminated plain UTF-8 text
    char *ass;         ///< 0-terminated ASS/SSA compatible event line.

} AVSubtitleArea;

typedef struct AVSubtitleRect {
    unsigned nb_refs;
    int x;         ///< top left corner  of pict, undefined when pict is not set
    int y;         ///< top left corner  of pict, undefined when pict is not set
    int w;         ///< width            of pict, undefined when pict is not set
    int h;         ///< height           of pict, undefined when pict is not set
    int nb_colors; ///< number of colors in pict, undefined when pict is not set

    /**
     * data+linesize for the bitmap of this subtitle.
     */
    uint8_t *data[4];
    int linesize[4];

    enum AVSubtitleType type;

    char *text;                     ///< 0 terminated plain UTF-8 text

    /**
     * 0-terminated ASS/SSA compatible event line.
     */
    char *ass;

    int flags;
} AVSubtitleRect;

typedef struct AVSubtitle {
    uint16_t format; /* 0 = graphics */
    uint32_t start_display_time; /* relative to packet pts, in ms */
    uint32_t end_display_time; /* relative to packet pts, in ms */
    unsigned num_rects;
    AVSubtitleRect **rects;
    int64_t pts;    ///< Same as packet pts, in AV_TIME_BASE
} AVSubtitle;

/**
 * Return the name of sub_fmt, or NULL if sub_fmt is not
 * recognized.
 */
const char *av_get_subtitle_fmt_name(enum AVSubtitleType sub_fmt);

/**
 * Return a subtitle format corresponding to name, or AV_SUBTITLE_FMT_NONE
 * on error.
 *
 * @param name Subtitle format name.
 */
enum AVSubtitleType av_get_subtitle_fmt(const char *name);

/**
 * Copy a subtitle area.
 *
 * @param dst        The target area.
 * @param src        The source area.
 * @param copy_data  Determines whether to copy references or actual
 *                   data from @ref AVSubtitleArea.buf.
 *
 * @return 0 on success.
 */
int av_subtitle_area2area(AVSubtitleArea *dst, const AVSubtitleArea *src, int copy_data);

/**
 * Copy data from @ref AVSubtitleArea to @ref AVSubtitleRect.
 *
 * @param dst        The target rect (@ref AVSubtitleRect).
 * @param src        The source area (@ref AVSubtitleArea).
 *
 * @return 0 on success.
 *
 * @deprecated This is a compatibility method for interoperability with
 * the legacy subtitle API.
 */
int av_subtitle_area2rect(AVSubtitleRect *dst, const AVSubtitleArea *src);

/**
 * Copy data from @ref AVSubtitleRect to @ref AVSubtitleArea.
 *
 * @param dst        The source area (@ref AVSubtitleArea).
 * @param src        The target rect (@ref AVSubtitleRect).
 *
 * @return 0 on success.
 *
 * @deprecated This is a compatibility method for interoperability with
 * the legacy subtitle API.
 */
int av_subtitle_rect2area(AVSubtitleArea *dst, const AVSubtitleRect *src);

/**
 * Free all allocated data in the given subtitle struct.
 *
 * @param sub AVSubtitle to free.
 */
void avsubtitle_free(AVSubtitle *sub);

#endif /* AVUTIL_SUBFMT_H */
