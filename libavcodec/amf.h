#ifndef AV_AMF_H
#define AV_AMF_H
#include <AMF/core/Surface.h>
#include "libavformat/avformat.h"

/**
* Error handling helper
*/
#define AMF_RETURN_IF_FALSE(avctx, exp, ret_value, /*message,*/ ...) \
    if (!(exp)) { \
        av_log(avctx, AV_LOG_ERROR, __VA_ARGS__); \
        return ret_value; \
    }

#define AMFAV_GOTO_FAIL_IF_FALSE(avctx, exp, ret_value, /*message,*/ ...) \
    if (!(exp)) { \
        av_log(avctx, AV_LOG_ERROR, __VA_ARGS__); \
        ret = ret_value; \
        goto fail; \
    }

#define AMF_TIME_BASE_Q          (AVRational){1, AMF_SECOND}

typedef struct FormatMap {
    enum AVPixelFormat       av_format;
    enum AMF_SURFACE_FORMAT  amf_format;
} FormatMap;

extern const FormatMap format_map[];
enum AMF_SURFACE_FORMAT amf_av_to_amf_format(enum AVPixelFormat fmt);
enum AVPixelFormat amf_to_av_format(enum AMF_SURFACE_FORMAT fmt);

/**
* Supported formats
*/
extern const enum AVPixelFormat ff_amf_pix_fmts[];

#endif // AV_AMF_H
