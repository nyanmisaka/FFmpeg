#ifndef AVCODEC_AMFDEC_H
#define AVCODEC_AMFDEC_H
//#include "internal.h"

#include "libavutil/frame.h"
#include "libavutil/fifo.h"
#include "libavutil/hwcontext_amf.h"
#include "libavutil/opt.h"
#include "libavformat/avformat.h"
#include <AMF/core/Buffer.h>
#include <AMF/components/Component.h>
#include <AMF/core/Factory.h>
#include <AMF/core/Surface.h>
#include <AMF/core/Context.h>
#include "avcodec.h"
#include <AMF/components/VideoDecoderUVD.h>

/**
* AMF encoder context
*/

typedef struct AvAmfDecoderContext {
    AVClass            *avclass;

    AMFContext         *context;
    AMFFactory         *factory;
    AVBufferRef        *amf_device_ctx;

    //encoder
    AMFComponent       *decoder; ///< AMF decoder object
    AMF_SURFACE_FORMAT  format;  ///< AMF surface format

    AVBufferRef        *hw_device_ctx; ///< pointer to HW accelerator (decoder)
    AVBufferRef        *hw_frames_ctx; ///< pointer to HW accelerator (frame allocator)

    AVBufferRef        *hw_device_ref;
    AVBufferRef        *hw_frames_ref;

    // shift dts back by max_b_frames in timing
    AVFifoBuffer       *timestamp_list;
    int64_t             dts_delay;

    // common encoder option options

    int                 log_to_dbg;
    // Static options, have to be set before Init() call
    int                 decoder_mode;
    int                 timestamp_mode;

} AvAmfDecoderContext;

#endif // AVCODEC_AMFDEC_H
