/*
 * Misc image conversion routines
 * Copyright (c) 2001, 2002, 2003 Fabrice Bellard
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
 * misc image conversion routines
 */

#include "avcodec.h"
#include "libavutil/avassert.h"
#include "libavutil/pixdesc.h"
#include "libavutil/pixfmt.h"

#if FF_API_GETCHROMA
void avcodec_get_chroma_sub_sample(enum AVPixelFormat pix_fmt, int *h_shift, int *v_shift)
{
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(pix_fmt);
    av_assert0(desc);
    *h_shift = desc->log2_chroma_w;
    *v_shift = desc->log2_chroma_h;
}
#endif

#if FF_API_AVCODEC_PIX_FMT
int avcodec_get_pix_fmt_loss(enum AVPixelFormat dst_pix_fmt,
                             enum AVPixelFormat src_pix_fmt,
                             int has_alpha)
{
    return av_get_pix_fmt_loss(dst_pix_fmt, src_pix_fmt, has_alpha);
}

enum AVPixelFormat avcodec_find_best_pix_fmt_of_2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
                                            enum AVPixelFormat src_pix_fmt, int has_alpha, int *loss_ptr)
{
    return av_find_best_pix_fmt_of_2(dst_pix_fmt1, dst_pix_fmt2, src_pix_fmt, has_alpha, loss_ptr);
}

enum AVPixelFormat avcodec_find_best_pix_fmt2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
                                            enum AVPixelFormat src_pix_fmt, int has_alpha, int *loss_ptr)
{
    return av_find_best_pix_fmt_of_2(dst_pix_fmt1, dst_pix_fmt2, src_pix_fmt, has_alpha, loss_ptr);
}

#endif
enum AVPixelFormat avcodec_find_best_pix_fmt_of_list(const enum AVPixelFormat *pix_fmt_list,
                                            enum AVPixelFormat src_pix_fmt,
                                            int has_alpha, int *loss_ptr){
    int i;

    enum AVPixelFormat best = AV_PIX_FMT_NONE;
    int loss;

    for (i=0; pix_fmt_list[i] != AV_PIX_FMT_NONE; i++) {
        loss = loss_ptr ? *loss_ptr : 0;
        best = av_find_best_pix_fmt_of_2(best, pix_fmt_list[i], src_pix_fmt, has_alpha, &loss);
    }

    if (loss_ptr)
        *loss_ptr = loss;
    return best;
}
