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

__kernel void neighbor(__write_only image2d_t dst,
                       __read_only  image2d_t src)
{
    const sampler_t sampler = (CLK_NORMALIZED_COORDS_TRUE |
                               CLK_ADDRESS_CLAMP_TO_EDGE |
                               CLK_FILTER_NEAREST);

    int2 dst_pos = {get_global_id(0), get_global_id(1)};
    float2 dst_size = {get_global_size(0), get_global_size(1)};

    float2 src_coord = (convert_float2(dst_pos) + 0.5f) / dst_size;

    float4 c = read_imagef(src, sampler, src_coord);
    write_imagef(dst, dst_pos, c);
}

__kernel void scale(__write_only image2d_t dst,
                    __read_only  image2d_t src,
                    __constant   float    *cx,
                    __constant   float    *cy,
                                 int2      flt_size)
{
    const sampler_t s_img = (CLK_NORMALIZED_COORDS_FALSE |
                             CLK_ADDRESS_CLAMP_TO_EDGE |
                             CLK_FILTER_NEAREST);

    int2 dst_pos = {get_global_id(0), get_global_id(1)};

    float2 dst_size = {get_global_size(0), get_global_size(1)};
    float2 src_size = convert_float2(get_image_dim(src));

    float2 src_coord = (convert_float2(dst_pos) + 0.5f) * src_size / dst_size;

    int2 src_pos = convert_int2(floor(src_coord - 0.5f));

    float4 col = 0.0f;
    for (int i = 0; i < flt_size.y; ++i) {
        float4 s = 0.0f;
        for (int j = 0; j < flt_size.x; ++j) {
            float4 c = read_imagef(src, s_img, src_pos + (int2){flt_size.x/2 - j, flt_size.y/2 - i});
            s += c * cx[dst_pos.x * flt_size.x + j];
        }
        col += s * cy[dst_pos.y * flt_size.y + i];
    }

    write_imagef(dst, dst_pos, col);
}
