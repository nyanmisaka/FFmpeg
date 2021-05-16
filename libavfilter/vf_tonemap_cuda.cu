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

extern "C" {
const float REC709_ALPHA = 1.09929682680944f;
const float REC709_BETA = 0.018053968510807f;

const float SMPTE_240M_ALPHA = 1.111572195921731f;
const float SMPTE_240M_BETA  = 0.022821585529445f;

// Adjusted for continuity of first derivative.
const float SRGB_ALPHA = 1.055010718947587f;
const float SRGB_BETA = 0.003041282560128f;

const float ST2084_M1 = 0.1593017578125f;
const float ST2084_M2 = 78.84375f;
const float ST2084_C1 = 0.8359375f;
const float ST2084_C2 = 18.8515625f;
const float ST2084_C3 = 18.6875f;

const float ARIB_B67_A = 0.17883277f;
const float ARIB_B67_B = 0.28466892f;
const float ARIB_B67_C = 0.55991073f;

const float FLOAT_EPS = 1.175494351e-38f;

const float MP_REF_WHITE = 203.0f;
const float MP_REF_WHITE_HLG = 3.17955f;

// Common constants for SMPTE ST.2084 (HDR)
const float PQ_M1 = 2610.0f / 4096.0f * 1.0f / 4.0f;
const float PQ_M2 = 2523.0f / 4096.0f * 128.0f;
const float PQ_C1 = 3424.0f / 4096.0f;
const float PQ_C2 = 2413.0f / 4096.0f * 32.0f;
const float PQ_C3 = 2392.0f / 4096.0f * 32.0f;

#define ST2084_MAX_LUMINANCE 10000.0f
#define REFERENCE_WHITE 100.0f    
    
#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))
//#define clamp(x, low, high) (((x) <= (high)) ? (((x) >= (low)) ? (x) : (low)) : (high))

__device__ float eotf_st2084(float x) {
    float p = __powf(x, 1.0f / ST2084_M2);
    float a = __fmaxf(p -ST2084_C1, 0.0f);
    float b = __fmaxf(ST2084_C2 - ST2084_C3 * p, 1e-6f);
    float c  = __powf(a / b, 1.0f / ST2084_M1);
    return x > 0.0f ? c * ST2084_MAX_LUMINANCE / REFERENCE_WHITE : 0.0f;
}

__device__ float st_2084_eotf(float x) {
    // Filter negative values to avoid NAN.
    if (x > 0.0f) {
        float xpow = __powf(x, 1.0f / ST2084_M2);
        float num = __fmaxf(xpow - ST2084_C1, 0.0f);
        float den = __fmaxf(ST2084_C2 - ST2084_C3 * xpow, 1e-6f);
        x = __powf(num / den, 1.0f / ST2084_M1) * ST2084_MAX_LUMINANCE / REFERENCE_WHITE;
    } else {
        x = 0.0f;
    }

    return x;
}

__device__ float st_2084_inverse_eotf(float x) {
    // Filter negative values to avoid NAN, and also special-case 0 so that (f(g(0)) == 0).
    if (x > 0.0f) {
        float xpow = __powf(x, ST2084_M1);
#if 0
        // Original formulation from SMPTE ST 2084:2014 publication.
        float num = ST2084_C1 + ST2084_C2 * xpow;
        float den = 1.0f + ST2084_C3 * xpow;
        x = powf(num / den, ST2084_M2) * 10000;//ST2084_MAX_LUMINANCE / REFERENCE_WHITE;
#else
        // More stable arrangement that avoids some cancellation error.
        float num = (ST2084_C1 - 1.0f) + (ST2084_C2 - ST2084_C3) * xpow;
        float den = 1.0f + ST2084_C3 * xpow;
        x = __powf(1.0f + num / den, ST2084_M2) * 10000;//ST2084_MAX_LUMINANCE / REFERENCE_WHITE;
#endif
    } else {
        x = 0.0f;
    }

    return x;
}

__device__ float inverse_eotf_bt1886(float c) {
    return c < 0.0f ? 0.0f : __powf(c, 1.0f / 2.4f);
}


__device__ float linear_to_pq_space(float x) {
    if (x > 0.0f) {
        x *= MP_REF_WHITE / 10000.0f;
        x = __powf(x, PQ_M1);
        x = (PQ_C1 + PQ_C2 * x) / (1.0f + PQ_C3 * x);
        x = __powf(x, PQ_M2);
        return x;
    } else {
        return 0.0f;
    }
}

__device__ float pq_space_to_linear(float x) {
    if (x > 0.0f) {
        x = __powf(x, 1.0f / PQ_M2);
        x = __fmaxf(x - PQ_C1, 0.0f) / (PQ_C2 - PQ_C3 * x);
        x = __powf(x, 1.0f / PQ_M1);
        x *= 10000.0f / MP_REF_WHITE;
        x *= 10000;
        return x;
    } else {
        return 0.0f;
    }
}

__device__ float apply_bt2390(float x, const float maxLum) {
    const float ks = 1.5f * maxLum - 0.5f;
    float tb = (x - ks) / (1.0f - ks);
    float tb2 = tb * tb;
    float tb3 = tb2 * tb;
    float pb = (2.0f * tb3 - 3.0f * tb2 + 1.0f) * ks +
        (tb3 - 2.0f * tb2 + tb) * (1.0f - ks) +
        (-2.0f * tb3 + 3.0f * tb2) * maxLum;
    //x = mix(pb, x, lessThan(x, ks));
    x *= ST2084_MAX_LUMINANCE / REFERENCE_WHITE;
    x = (x < ks) ? x : pb;
    return x;
}

__device__ float hdr2sdr_reinhard(float x, float source_peak, float ldr_nits, float offset, float peak) {
    //const float eb = source_peak / ldr_nits;
    //peak *= eb;
    return x / (x + offset) * (peak + offset) / peak;
}

__device__ float mix(float x, float y, float a) {
    a = (a < 0.0f) ? 0.0f : a;
    a = (a > 1.0f) ? 1.0f : a;
    return (x) * (1.0f - (a)) + (y) * (a);
}

__global__ void Tonemap_Cuda(
    short* y_data, int y_linesize,
    short* uv_data, int uv_linesize,
    int width, int height,
    double yuv2rgb[][3], double rgb2yuv[][3], double rgb2rgb[][3])
{
    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;
    
    //816
    if (x < width && y < height)
    {
        //y_data[x + y * (y_linesize / 2)] *= 1.5;

        int idxY, idxU, idxV;
        float Y, U, V;
        idxY = y * (y_linesize / 2) + x;
        Y = y_data[idxY];
        
        if (x % 2 == 0)
        {
            idxU = y / 2 * (uv_linesize / 2) + x;
            idxV = y / 2 * (uv_linesize / 2) + x + 1;
            U = uv_data[idxU];
            V = uv_data[idxV];
        }
        else if (x % 2 == 1)
        {
            idxV = y / 2 * (uv_linesize / 2) + x;
            idxU = y / 2 * (uv_linesize / 2) + x - 1;
            U = uv_data[idxU];
            V = uv_data[idxV];
        }

        float sig_peak = 1000.0f;
        const float dst_peak = 100.0f;
        const float inv_dst_peak = 1.0f / dst_peak;
        
        const float sig_peak_pq = linear_to_pq_space(sig_peak);
        const float scale = 1.0 / sig_peak_pq;
        
        // full range in
        U -= 0.5f;
        V -= 0.5f;
        
        float R = (Y * yuv2rgb[0][0] + U * yuv2rgb[0][1] + V * yuv2rgb[0][2]);
        float G = (Y * yuv2rgb[1][0] + U * yuv2rgb[1][1] + V * yuv2rgb[1][2]);
        float B = (Y * yuv2rgb[2][0] + U * yuv2rgb[2][1] + V * yuv2rgb[2][2]);
        
        //R = (float)(R * (float)rgb2rgb[0][0] + G * (float)rgb2rgb[0][1] + B * (float)rgb2rgb[0][2]);
        //G = (float)(R * (float)rgb2rgb[1][0] + G * (float)rgb2rgb[1][1] + B * (float)rgb2rgb[1][2]);
        //B = (float)(R * (float)rgb2rgb[2][0] + G * (float)rgb2rgb[2][1] + B * (float)rgb2rgb[2][2]);
        
        //float R1 = R;
        //float G1 = G;
        //float B1 = B;

        R = st_2084_eotf(R);
        G = st_2084_eotf(G);
        B = st_2084_eotf(B);
        
        
        //R = clamp(R, 0.0f, 2550.0f);
        //G = clamp(G, 0.0f, 2550.0f);
        //B = clamp(B, 0.0f, 2550.0f);
        
        float sig = max(max(R, max(G, B)), 1e-6f);
        
        //int para = 10;
        //int peak = 100000;
        
        //sig = hdr2sdr_reinhard(R, 1000, 100, para, peak);
        //sig = min(sig, 1.0f);
        //R *= sig;
        //G *= sig;
        //B *= sig;
        
        
        //R = hdr2sdr_reinhard(R, 1000, 100, para, peak);
        //G = hdr2sdr_reinhard(G, 1000, 100, para, peak);
        //B = hdr2sdr_reinhard(B, 1000, 100, para, peak);
        
        //R = clamp(R, 0.0f, 1.0f);
        //G = clamp(G, 0.0f, 1.0f);
        //B = clamp(B, 0.0f, 1.0f);

        //R = inverse_eotf_bt1886(R);
        //G = inverse_eotf_bt1886(G);
        //B = inverse_eotf_bt1886(B);
        
        /*
        const float in_max  = fmaxf( fmaxf(R1, G1), fmaxf(B1, 1e-6f) );
        const float out_max = fmaxf( fmaxf(R, G), fmaxf(B, 1e-6f) );
        const float mul = out_max / in_max;
        const float desat_scale = 1.0f;
        const float desat_base = 0.18f;
        const float desat_strength = 0.75f;
        const float desat_exp = 1.5f;
        // in coeff calculation, "out_max" should be in normalized scale
        const float coeff = fmaxf(out_max * desat_scale - desat_base, 1e-6f) / fmaxf(out_max * desat_scale, 1.0f);
        const float mixcoeff = desat_strength * powf(coeff, desat_exp);
        R = mix(R1 * mul, R, mixcoeff);
        G = mix(G1 * mul, G, mixcoeff);
        B = mix(B1 * mul, B, mixcoeff);
        */
        
        // use non-normalized value
        //R *= dst_peak;
        //G *= dst_peak;
        //B *= dst_peak;
        
        //R = linear_to_pq_space(R);// * scale;
        //G = linear_to_pq_space(G);// * scale;
        //B = linear_to_pq_space(B);// * scale;
        //const float maxLum = linear_to_pq_space(dst_peak) * scale;
        
        //R = apply_bt2390(R, maxLum) * sig_peak_pq;
        //G = apply_bt2390(G, maxLum) * sig_peak_pq;
        //B = apply_bt2390(B, maxLum) * sig_peak_pq;
        
        //R = pq_space_to_linear(R);
        //G = pq_space_to_linear(G);
        //B = pq_space_to_linear(B);
        
        //R *= inv_dst_peak;
        //G *= inv_dst_peak;
        //B *= inv_dst_peak;

        Y = (R * rgb2yuv[0][0] + G * rgb2yuv[0][1] + B * rgb2yuv[0][2]);
        U = (R * rgb2yuv[1][0] + G * rgb2yuv[1][1] + B * rgb2yuv[1][2]);
        V = (R * rgb2yuv[2][0] + G * rgb2yuv[2][1] + B * rgb2yuv[2][2]);
        
        U += 0.5f;
        V += 0.5f;
        
        //Y = R;//(float)(0.299 * R + 0.587 * G + 0.114 * B);
        //U = G;//(float)(-0.147 * R - 0.289 * G + 0.436 * B + 128);
        //V = B;//(float)(0.615 * R - 0.368 * G - 0.071 * B + 128);
        
        //unsigned char R, G, B;
        //limited range
        //Y = (Y * 255.0f -  16.0f) / 219.0f;
        //U = (U * 255.0f - 128.0f) / 224.0f;
        //V = (V * 255.0f - 128.0f) / 224.0f;
        //R = (unsigned char)(Y * (float)yuv2rgb[0][0] + U * (float)yuv2rgb[0][1] + V * (float)yuv2rgb[0][2]);
        //G = (unsigned char)(Y * (float)yuv2rgb[1][0] + U * (float)yuv2rgb[1][1] + V * (float)yuv2rgb[1][2]);
        //B = (unsigned char)(Y * (float)yuv2rgb[2][0] + U * (float)yuv2rgb[2][1] + V * (float)yuv2rgb[2][2]);

        //R = (unsigned char)(Y + 1.402 * (V - 128));
        //G = (unsigned char)(Y - 0.34413 * (U - 128) - 0.71414 * (V - 128));
        //B = (unsigned char)(Y + 1.772 * (U - 128));

        //unsigned char YO = (unsigned char)(R * (float)rgb2yuv[0][0] + G * (float)rgb2yuv[0][1] + B * (float)rgb2yuv[0][2]);
        //unsigned char UO = (unsigned char)(R * (float)rgb2yuv[1][0] + G * (float)rgb2yuv[1][1] + B * (float)rgb2yuv[1][2]);
        //unsigned char VO = (unsigned char)(R * (float)rgb2yuv[2][0] + G * (float)rgb2yuv[2][1] + B * (float)rgb2yuv[2][2]);
        //YO = (219.0f * YO + 16.0f) / 255.0f;
        //UO = (224.0f * UO + 128.0f) / 255.0f;
        //VO = (224.0f * VO + 128.0f) / 255.0f;
        
        //unsigned char YO = (unsigned char)(0.299 * R + 0.587 * G + 0.114 * B);
        //unsigned char UO = (unsigned char)(-0.147 * R - 0.289 * G + 0.436 * B + 128);
        //unsigned char VO = (unsigned char)(0.615 * R - 0.368 * G - 0.071 * B + 128);

        //Y *= 1.5;
        //U *= 1.5;
        //V *= 1.5;
        
        y_data[x + y * (y_linesize / 2)] = Y;
        if (x % 2 == 0)
        {
            idxU = y / 2 * (uv_linesize / 2) + x;
            idxV = y / 2 * (uv_linesize / 2) + x + 1;
            uv_data[idxU] = U;
            uv_data[idxV] = V;
        }
        else if (x % 2 == 1)
        {
            idxV = y / 2 * (uv_linesize / 2) + x;
            idxU = y / 2 * (uv_linesize / 2) + x - 1;
            uv_data[idxU] = U;
            uv_data[idxV] = V;
        }
        
        //int x2 = x >> 1;
        //uv[x2] = (x&1) ? v : u;
    }
    
    //main[x + y * main_linesize] *= 0.8;
}

}
