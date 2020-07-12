/*
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


#ifndef AVUTIL_HWCONTEXT_AMF_H
#define AVUTIL_HWCONTEXT_AMF_H

/**
 * @file
 * API-specific header for AV_HWDEVICE_TYPE_AMF.
 *
 */

#include "frame.h"
#include "AMF/core/Context.h"
#include "AMF/core/Factory.h"

enum AMF_VIDEO_ENCODER_PREFFERED_ENGINE
{
    AMF_VIDEO_ENCODER_ENGINE_DEFAULT = 0,
    AMF_VIDEO_ENCODER_ENGINE_DXVA2,
    AMF_VIDEO_ENCODER_ENGINE_D3D11,
    AMF_VIDEO_ENCODER_ENGINE_VULKAN
};

/**
 * This struct is allocated as AVHWDeviceContext.hwctx
 */
typedef struct AVAMFDeviceContext {
    /**
     * Context used for:
     * texture and buffers allocation.
     * Access to device objects (DX9, DX11, OpenCL, OpenGL) which are being used in the context
     */
    AMFContext *context;

    /**
     * Factory used for:
     * AMF component creation such as encoder, decoder, converter...
     * Access AMF Library settings such as trace/debug/cache
     */
    AMFFactory *factory;
} AVAMFDeviceContext;


#endif /* AVUTIL_HWCONTEXT_AMF_H */
