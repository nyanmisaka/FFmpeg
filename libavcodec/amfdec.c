#include "amfdec.h"
#include <AMF/core/Variant.h>
#include <AMF/core/PropertyStorage.h>
#include <AMF/components/FFMPEGFileDemuxer.h>
#include "libavutil/imgutils.h"
#include "libavutil/time.h"
#include "amf.h"

#define propNotFound 0

static void amf_free_amfsurface(void *opaque, uint8_t *data)
{
    AMFSurface *surface = (AMFSurface*)(opaque);
    surface->pVtbl->Release(surface);
}

static int amf_init_decoder(AVCodecContext *avctx)
{
    AvAmfDecoderContext        *ctx = avctx->priv_data;
    const wchar_t     *codec_id = NULL;
    AMF_RESULT         res;
    enum AMF_SURFACE_FORMAT formatOut = AMF_SURFACE_P010;
    AMFBuffer * buffer;
    //enum AVPixelFormat pix_fmt = avctx->sw_pix_fmt;

    switch (avctx->codec->id) {
        case AV_CODEC_ID_H264:
            codec_id = AMFVideoDecoderUVD_H264_AVC;
            break;
        case AV_CODEC_ID_HEVC:
            codec_id = AMFVideoDecoderHW_H265_MAIN10;
            break;
        default:
            break;
    }
    AMF_RETURN_IF_FALSE(ctx, codec_id != NULL, AVERROR(EINVAL), "Codec %d is not supported\n", avctx->codec->id);

    res = ctx->factory->pVtbl->CreateComponent(ctx->factory, ctx->context, codec_id, &ctx->decoder);
    AMF_RETURN_IF_FALSE(ctx, res == AMF_OK, AVERROR_ENCODER_NOT_FOUND, "CreateComponent(%ls) failed with error %d\n", codec_id, res);

    AMF_ASSIGN_PROPERTY_INT64(res, ctx->decoder, AMF_TIMESTAMP_MODE, AMF_TS_DECODE);// our sample H264 parser provides decode order timestamps - change this depend on demuxer

    if (avctx->extradata_size)
    { // set SPS/PPS extracted from stream or container; Alternatively can use parser->SetUseStartCodes(true)
        ctx->context->pVtbl->AllocBuffer(ctx->context, AMF_MEMORY_HOST, avctx->extradata_size, &buffer);

        memcpy(buffer->pVtbl->GetNative(buffer), avctx->extradata, avctx->extradata_size);
        AMF_ASSIGN_PROPERTY_INTERFACE(res,ctx->decoder, AMF_VIDEO_DECODER_EXTRADATA, buffer);
    }

    res = ctx->decoder->pVtbl->Init(ctx->decoder, formatOut, avctx->width, avctx->height);// parser->GetPictureWidth(), parser->GetPictureHeight()
    return 0;
}

static int amf_init_decoder_context(AVCodecContext *avctx)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;
    AVAMFDeviceContext *amf_ctx;
    int ret;
    int err;

    if (avctx->hw_frames_ctx)
    {
        err = av_hwdevice_ctx_create_derived(&ctx->amf_device_ctx, AV_HWDEVICE_TYPE_AMF, avctx->hw_device_ctx, 0);
        if (err < 0)
            return err;
        ctx->hw_device_ref = av_buffer_ref(avctx->hw_device_ctx);
        if (!ctx->hw_device_ref)
            return AVERROR(ENOMEM);

        ctx->hw_frames_ref = av_hwframe_ctx_alloc(ctx->hw_device_ref);
        if (!ctx->hw_frames_ref)
            return AVERROR(ENOMEM);
    }
    else
    {
        ret = av_hwdevice_ctx_create(&ctx->amf_device_ctx, AV_HWDEVICE_TYPE_AMF, NULL, NULL, 0);
        if (ret < 0)
            return ret;
    }

    amf_ctx = ((AVHWDeviceContext*)ctx->amf_device_ctx->data)->hwctx;
    ctx->context = amf_ctx->context;
    ctx->factory = amf_ctx->factory;

    return ret;
}

static int ff_amf_decode_close(AVCodecContext *avctx)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;

    if (ctx->decoder) {
        ctx->decoder->pVtbl->Terminate(ctx->decoder);
        ctx->decoder->pVtbl->Release(ctx->decoder);
        ctx->decoder = NULL;
    }

    av_buffer_unref(&ctx->hw_device_ctx);
    av_buffer_unref(&ctx->hw_frames_ctx);

    ctx->factory = NULL;
    ctx->context = NULL;
    av_fifo_freep(&ctx->timestamp_list);

    av_buffer_unref(&ctx->amf_device_ctx);

    return 0;

}

static int ff_amf_decode_init(AVCodecContext *avctx)
{
    int ret;

    if ((ret = amf_init_decoder_context(avctx)) == 0) {
        if ((ret = amf_init_decoder(avctx)) == 0) {
            return 0;
        }
    }
    ff_amf_decode_close(avctx);
    return ret;
}

static void dumpAvFrame(char * path, const AVFrame *frame)
{
    FILE *fp;
    fp = fopen(path, "ab");
    if(!fp)
       return;
    fprintf(fp,"{\n");
    fprintf(fp, "    \"best_effort_timestamp\": %d,\n", frame->best_effort_timestamp);
    fprintf(fp, "    \"best_effort_timestamp\": %d,\n", frame->best_effort_timestamp);
    fprintf(fp, "    \"channel_layout\": %d,\n", (int)frame->channel_layout);
    fprintf(fp, "    \"channels\": %d,\n", frame->channels);
    fprintf(fp, "    \"coded_picture_number\": %d,\n", frame->coded_picture_number);
    fprintf(fp, "    \"crop_bottom\": %d,\n", (int)frame->crop_bottom);
    fprintf(fp, "    \"crop_left\": %d,\n", (int)frame->crop_left);
    fprintf(fp, "    \"crop_right\": %d,\n", (int)frame->crop_right);
    fprintf(fp, "    \"crop_top\": %d,\n", (int)frame->crop_top);
    fprintf(fp, "    \"decode_error_flags\": %d,\n", frame->decode_error_flags);
    fprintf(fp, "    \"display_picture_number\": %d,\n", frame->display_picture_number);
    fprintf(fp, "    \"flags\": %d,\n", frame->flags);
    fprintf(fp, "    \"format\": %d,\n", frame->format);
    fprintf(fp, "    \"height\": %d,\n", frame->height);
    fprintf(fp, "    \"interlaced_frame\": %d,\n", frame->interlaced_frame);
    fprintf(fp, "    \"key_frame\": %d,\n", frame->key_frame);
    fprintf(fp, "    \"nb_extended_buf\": %d,\n", frame->nb_extended_buf);
    fprintf(fp, "    \"nb_samples\": %d,\n", frame->nb_samples);
    fprintf(fp, "    \"nb_side_data\": %d,\n", frame->nb_side_data);
    fprintf(fp, "    \"palette_has_changed\": %d,\n", frame->palette_has_changed);
    fprintf(fp, "    \"pict_type\": %d,\n", (int)frame->pict_type);
    fprintf(fp, "    \"pkt_dts\": %d,\n", frame->pkt_dts);
    fprintf(fp, "    \"pkt_duration\": %d,\n", frame->pkt_duration);
    fprintf(fp, "    \"pkt_pos\": %d,\n", frame->pkt_pos);
    fprintf(fp, "    \"pkt_size\": %d,\n", frame->pkt_size);
    fprintf(fp, "    \"pts\": %d,\n", frame->pts);
    fprintf(fp, "    \"quality\": %d,\n", frame->quality);
    fprintf(fp, "    \"reordered_opaque\": %d,\n", frame->reordered_opaque);
    fprintf(fp, "    \"repeat_pict\": %d,\n", frame->repeat_pict);
    fprintf(fp, "    \"sample_rate\": %d,\n", frame->sample_rate);
    fprintf(fp, "    \"top_field_first\": %d,\n", frame->top_field_first);
    fprintf(fp, "    \"width\": %d,\n", frame->width);
    fprintf(fp, "    \"sample_aspect_ratio_den\": %d,\n", frame->sample_aspect_ratio.den);
    fprintf(fp, "    \"sample_aspect_ratio_num\": %d,\n", frame->sample_aspect_ratio.num);

    fprintf(fp, "    \"color_range\": %d,\n", (int)frame->color_range);
    fprintf(fp, "    \"color_trc\": %d,\n", (int)frame->color_trc);
    fprintf(fp, "    \"colorspace\": %d,\n", (int)frame->colorspace);
    fprintf(fp, "    \"pict_type\": %d,\n", (int)frame->pict_type);
    fprintf(fp,"}\n");

    fclose(fp);
}

static void dumpAMFSurface(char * path, const AMFData *surface)
{
    FILE *fp;
    fp = fopen(path, "ab");
    if(!fp)
       return;
    AMF_RESULT res = AMF_OK;

    int count = surface->pVtbl->GetPropertyCount(surface);
    fprintf(fp,"{\n");
    fprintf(fp, "\"count \": %d,\n", count);
    int i;
    AMFVariantStruct    var = {0};
    res = AMFVariantInit(&var);

    for (i = 0; i < count; ++i)
    {
        wchar_t name[100];
        amf_size nameSize = 40;

        res = surface->pVtbl->GetPropertyAt(surface, i, &name, nameSize, &var);

        switch (var.type) {
        case AMF_VARIANT_BOOL:
            fprintf(fp, "    \"%S\":  %s,\n", name, var.boolValue == true?"true":"false");
            break;
        case AMF_VARIANT_INT64:
            fprintf(fp, "    \"%S\":  %d,\n", name, var.int64Value);
            break;
        case AMF_VARIANT_DOUBLE:
            fprintf(fp, "    \"%S\":  %f,\n", name, var.doubleValue);
            break;
        case AMF_VARIANT_STRING:
            fprintf(fp, "    \"%S\":  \"%s\",\n", name, var.stringValue);
            break;
        case AMF_VARIANT_WSTRING:
            fprintf(fp, "    \"%S\":  %S\n", name, var.wstringValue);
            break;
        case AMF_VARIANT_RATE:
            fprintf(fp, "    \"%S\":  \"%d:%d\",\n", name, var.rateValue.num, var.rateValue.den);
            break;
        case AMF_VARIANT_RATIO:
            fprintf(fp, "    \"%S\":  \"%d:%d\",\n", name, var.ratioValue.num, var.ratioValue.den);
            break;
        case AMF_VARIANT_SIZE:
            fprintf(fp, "    \"%S\":  \"%d - %d\",\n", name, var.sizeValue.width, var.sizeValue.height);
            break;
        case AMF_VARIANT_POINT:
            fprintf(fp, "    \"%S\":  \"%d - %d\",\n", name, var.pointValue.x, var.pointValue.y);
            break;
        case AMF_VARIANT_COLOR:
            fprintf(fp, "    \"%S\":  \"%d.%d.%d.%d\",\n", name, var.colorValue.r, var.colorValue.g, var.colorValue.b, var.colorValue.a);
            break;
        default:
            fprintf(fp, "    \"%S\":  \"%s\",\n", name, "type_AMFInterface -----------------------------------------");
            break;
        }
    }
    fprintf(fp,"}\n");
    fclose(fp);
}

static int amf_amfsurface_to_avframe(AVCodecContext *avctx, const AMFSurface* pSurface, AVFrame *frame)
{
    AMFPlane *plane;
    AMFVariantStruct var = {0};
    int       i;
    AMF_RESULT  ret = AMF_OK;

    if (!frame)
        return AMF_INVALID_POINTER;

//    switch (pSurface->pVtbl->GetMemoryType(pSurface))
//        {
//    #if CONFIG_D3D11VA
//            case AMF_MEMORY_DX11:
//            {
//                AMFPlane *plane0 = pSurface->pVtbl->GetPlaneAt(pSurface, 0);
//                frame->data[0] = plane0->pVtbl->GetNative(plane0);
//                frame->data[1] = (uint8_t*)(intptr_t)0;

//                frame->buf[0] = av_buffer_create(NULL,
//                                         0,
//                                         amf_free_amfsurface,
//                                         pSurface,
//                                         AV_BUFFER_FLAG_READONLY);
//                pSurface->pVtbl->Acquire(pSurface);
//            }
//            break;
//    #endif
//    #if CONFIG_DXVA2
//            case AMF_MEMORY_DX9:
//            {
//                AMFPlane *plane0 = pSurface->pVtbl->GetPlaneAt(pSurface, 0);
//                frame->data[3] = plane0->pVtbl->GetNative(plane0);

//                frame->buf[0] = av_buffer_create(NULL,
//                                         0,
//                                         amf_free_amfsurface,
//                                         pSurface,
//                                         AV_BUFFER_FLAG_READONLY);
//                pSurface->pVtbl->Acquire(pSurface);
//            }
//            break;
//    #endif
//        default:
//            {
                ret = pSurface->pVtbl->Convert(pSurface, AMF_MEMORY_HOST);
                AMF_RETURN_IF_FALSE(avctx, ret == AMF_OK, AMF_UNEXPECTED, L"Convert(amf::AMF_MEMORY_HOST) failed with error %d\n");

                for (i = 0; i < pSurface->pVtbl->GetPlanesCount(pSurface); i++)
                {
                    plane = pSurface->pVtbl->GetPlaneAt(pSurface, i);
                    frame->data[i] = plane->pVtbl->GetNative(plane);
                    frame->linesize[i] = plane->pVtbl->GetHPitch(plane);
                }
                pSurface->pVtbl->Acquire(pSurface);
                frame->buf[0] = av_buffer_create(NULL,
                                                     0,
                                                     amf_free_amfsurface,
                                                     pSurface,
                                                     AV_BUFFER_FLAG_READONLY);
//            }
//        }

//    for (i = 0; i < pSurface->pVtbl->GetPlanesCount(pSurface); i++)
//    {
//        frame->linesize[i] = plane->pVtbl->GetHPitch(plane);
//    }

    frame->color_trc = AVCOL_TRC_SMPTE2084;
    frame->colorspace = AVCOL_SPC_BT2020_NCL;
    frame->color_range = AVCOL_RANGE_MPEG;
    frame->color_primaries = AVCOL_PRI_BT2020;
    frame->format = amf_to_av_format(pSurface->pVtbl->GetFormat(pSurface));
    frame->width  = avctx->width;
    frame->height = avctx->height;

    frame->pkt_pts = pSurface->pVtbl->GetPts(pSurface);

    pSurface->pVtbl->GetProperty(pSurface, L"FFMPEG:dts", &var);
    frame->pkt_dts = var.int64Value;

    pSurface->pVtbl->GetProperty(pSurface, L"FFMPEG:size", &var);
    frame->pkt_size = var.int64Value;

    pSurface->pVtbl->GetProperty(pSurface, L"FFMPEG:duration", &var);
    frame->pkt_duration = var.int64Value;
    frame->pkt_duration = pSurface->pVtbl->GetDuration(pSurface);

    pSurface->pVtbl->GetProperty(pSurface, L"FFMPEG:pos", &var);
    frame->pkt_pos = var.int64Value;

    dumpAvFrame("e:/tmp/frames/amfdec.json", frame);
    dumpAMFSurface("e:/tmp/frames/amfdec_surf.json", pSurface);
    return ret;
}

static int ff_amf_receive_frame(const AVCodecContext *avctx, AVFrame *frame)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;
    AMF_RESULT  ret;
    AMFSurface *surface = NULL;
    AVFrame *data = NULL;
    AMFData *data_out = NULL;

    if (!ctx->decoder)
        return AVERROR(EINVAL);

    ret = ctx->decoder->pVtbl->QueryOutput(ctx->decoder, &data_out);
    AMFAV_GOTO_FAIL_IF_FALSE(avctx, ret == AMF_OK, AVERROR_UNKNOWN, "QueryOutput() failed with error %d\n", ret);

    AMFAV_GOTO_FAIL_IF_FALSE(avctx, data_out, AVERROR_UNKNOWN, "QueryOutput() return empty data %d\n", ret);

    if (data_out)
    {
        AMFGuid guid = IID_AMFSurface();
        data_out->pVtbl->QueryInterface(data_out, &guid, (void**)&surface); // query for buffer interface
        data_out->pVtbl->Release(data_out);
    }

    data = av_frame_alloc();
    ret = amf_amfsurface_to_avframe(avctx, surface, data);
    AMFAV_GOTO_FAIL_IF_FALSE(avctx, ret == AMF_OK, AVERROR_UNKNOWN, "Failed to convert AMFSurface to AVFrame", ret);

    av_frame_move_ref(frame, data);
fail:
    if (data) {
        av_frame_free(&data);
    }
    if (surface){
        surface->pVtbl->Release(surface);
    }
    return ret;
}

static void AMF_STD_CALL  update_buffer_video_duration(AVCodecContext *avctx, AMFBuffer* pBuffer, const AVPacket* pPacket)
{
    if (pPacket->duration != 0)
    {
        amf_int64 durationByFFMPEG    = av_rescale_q(pPacket->duration, avctx->time_base, AMF_TIME_BASE_Q);
        amf_int64 durationByFrameRate = (amf_int64)((amf_double)AMF_SECOND / ((amf_double)avctx->framerate.num / (amf_double)avctx->framerate.den));
        if (abs(durationByFrameRate - durationByFFMPEG) > AMF_MIN(durationByFrameRate, durationByFFMPEG) / 2)
        {
            durationByFFMPEG = durationByFrameRate;
        }

        pBuffer->pVtbl->SetDuration(pBuffer, durationByFFMPEG);
    }
}

static AMF_RESULT amf_update_buffer_properties(AVCodecContext *avctx, AMFBuffer* pBuffer, const AVPacket* pPacket)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;
    AMFContext *ctxt = ctx->context;
    AMF_RESULT res;

    AMF_RETURN_IF_FALSE(ctxt, pBuffer != NULL, AMF_INVALID_ARG, "UpdateBufferProperties() - buffer not passed in");
    AMF_RETURN_IF_FALSE(ctxt, pPacket != NULL, AMF_INVALID_ARG, "UpdateBufferProperties() - packet not passed in");

    const amf_int64  pts = av_rescale_q(pPacket->dts, avctx->time_base, AMF_TIME_BASE_Q);
    pBuffer->pVtbl->SetPts(pBuffer, pts);// - GetMinPosition());
    if (pPacket != NULL)
    {
        //AMFVariantStruct var;
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:pts", pPacket->pts);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:dts", pPacket->dts);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:stream_index", pPacket->stream_index);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:flags", pPacket->flags);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:duration", pPacket->duration);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:size", pPacket->size);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:pos", pPacket->pos);
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:convergence_duration", pPacket->convergence_duration);
    }
    int m_ptsInitialMinPosition = propNotFound;
    AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:FirstPtsOffset", m_ptsInitialMinPosition);

    int vst = propNotFound;//avctx->vst
    if (vst != AV_NOPTS_VALUE)
    {
        int start_time = propNotFound;
        AMF_ASSIGN_PROPERTY_INT64(res, pBuffer, L"FFMPEG:start_time", start_time);
    }

    AMF_ASSIGN_PROPERTY_DATA(res, Int64, pBuffer, L"FFMPEG:time_base_den", avctx->time_base.den);
    AMF_ASSIGN_PROPERTY_DATA(res, Int64, pBuffer, L"FFMPEG:time_base_num", avctx->time_base.num);

    int condition1 = propNotFound;//(m_iVideoStreamIndex == -1 || pPacket->stream_index == m_iVideoStreamIndex) && m_ptsSeekPos != -1
    if (condition1)
    {
        int m_ptsSeekPos = propNotFound;
        if (pts < m_ptsSeekPos)
        {
            AMF_ASSIGN_PROPERTY_BOOL(res, pBuffer, L"Seeking", true);
        }
        int m_ptsPosition = propNotFound;
        if (m_ptsSeekPos <= m_ptsPosition)
        {
            AMF_ASSIGN_PROPERTY_BOOL(res, pBuffer, L"EndSeeking", true);

            AVFormatContext * m_pInputContext = propNotFound;
            int default_stream_index = av_find_default_stream_index(m_pInputContext);
            if (pPacket->stream_index == default_stream_index)
            {
                m_ptsSeekPos = -1;
            }
        }
        else
        {
            if (pPacket->flags & AV_PKT_FLAG_KEY)
            {
                AMF_ASSIGN_PROPERTY_BOOL(res, pBuffer, L"BeginSeeking", true);
            }
        }
    }
    AMF_ASSIGN_PROPERTY_DATA(res, Int64, pBuffer, FFMPEG_DEMUXER_BUFFER_TYPE, AMF_STREAM_VIDEO);
    update_buffer_video_duration(avctx, pBuffer, pPacket);

    AMF_ASSIGN_PROPERTY_DATA(res, Int64, pBuffer, FFMPEG_DEMUXER_BUFFER_STREAM_INDEX, pPacket->stream_index);
    AMF_RETURN_IF_FALSE(res == AMF_OK, ctxt, res, L"Failed to set property");
    return AMF_OK;
}

static AMF_RESULT amf_buffer_from_packet(AVCodecContext *avctx, const AVPacket* pPacket, AMFBuffer** ppBuffer)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;
    AMFContext *ctxt = ctx->context;
    void *pMem;
    AMF_RESULT err;

    AMF_RETURN_IF_FALSE(ctxt, pPacket != NULL, AMF_INVALID_ARG, L"BufferFromPacket() - packet not passed in");
    AMF_RETURN_IF_FALSE(ctxt, ppBuffer != NULL, AMF_INVALID_ARG, L"BufferFromPacket() - buffer pointer not passed in");


    // Reproduce FFMPEG packet allocate logic (file libavcodec/avpacket.c function av_packet_duplicate)
    // ...
    //    data = av_malloc(pkt->size + FF_INPUT_BUFFER_PADDING_SIZE);
    // ...
    //MM this causes problems because there is no way to set real buffer size. Allocation has 32 byte alignment - should be enough.
    err = ctxt->pVtbl->AllocBuffer(ctxt, AMF_MEMORY_HOST, pPacket->size + AV_INPUT_BUFFER_PADDING_SIZE, ppBuffer);
    AMF_RETURN_IF_FALSE(ctxt, err == AMF_OK, err, L"BufferFromPacket() - AllocBuffer failed");

    AMFBuffer* pBuffer = *ppBuffer;
    err = pBuffer->pVtbl->SetSize(pBuffer, pPacket->size);
    AMF_RETURN_IF_FALSE(ctxt, err == AMF_OK, err, L"BufferFromPacket() - SetSize failed");

    // get the memory location and check the buffer was indeed allocated
    pMem = pBuffer->pVtbl->GetNative(pBuffer);
    AMF_RETURN_IF_FALSE(ctxt, pMem != NULL, AMF_INVALID_POINTER, "BufferFromPacket() - GetMemory failed");

    // copy the packet memory and don't forget to
    // clear data padding like it is done by FFMPEG
    memcpy(pMem, pPacket->data, pPacket->size);
    memset((amf_int8*)(pMem)+pPacket->size, 0, AV_INPUT_BUFFER_PADDING_SIZE);

    // now that we created the buffer, it's time to update
    // it's properties from the packet information...
    return amf_update_buffer_properties(avctx, pBuffer, pPacket);
}

static int amf_decode_frame(AVCodecContext *avctx, void *data,
                       int *got_frame, AVPacket *avpkt)
{
    AvAmfDecoderContext *ctx = avctx->priv_data;

    AMFBuffer * buf;
    AMF_RESULT res;
    AVFrame *frame = data;

    if (!avpkt->size)
    {
        res = ff_amf_receive_frame(avctx, frame);
        if (res == AMF_OK)
        {
            *got_frame = 1;
        }
        return 0;
    }

    res = amf_buffer_from_packet(avctx, avpkt, &buf);
    AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, 0, "Cannot convert AVPacket to AMFbuffer");

    while(1)
    {
        res = ctx->decoder->pVtbl->SubmitInput(ctx->decoder, buf);
        if (res == AMF_OK)
        {
            break;
        }
        else if (res == AMF_INPUT_FULL || res == AMF_DECODER_NO_FREE_SURFACES)
        {
            res = ff_amf_receive_frame(avctx, frame);
            if (res == AMF_OK)
            {
                AMF_RETURN_IF_FALSE(avctx, !*got_frame, avpkt->size, "frame already got");
                *got_frame = 1;
                av_usleep(1);
            }
        }
        else
        {
            AMF_RETURN_IF_FALSE(avctx, res == AMF_OK, 0, "SubmitInput to decoder failed");
        }
    }

    return avpkt->size;
}

static void amf_decode_flush(AVCodecContext *avctx)
{
    AMF_RESULT res = AMF_OK;
    AvAmfDecoderContext *ctx = avctx->priv_data;
    res = ctx->decoder->pVtbl->Flush(ctx->decoder);
}

#define OFFSET(x) offsetof(AvAmfDecoderContext, x)
#define VD AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_DECODING_PARAM

static const AVOption options[] = {
    //Decoder mode
    { "decoder_mode",          "Decoder mode",        OFFSET(decoder_mode),  AV_OPT_TYPE_INT,   { .i64 = AMF_VIDEO_DECODER_MODE_REGULAR      }, AMF_VIDEO_DECODER_MODE_REGULAR, AMF_VIDEO_DECODER_MODE_LOW_LATENCY, VD, "decoder_mode" },
    { "timestamp_mode",        "Timestamp mode",        OFFSET(timestamp_mode),  AV_OPT_TYPE_INT,   { .i64 = AMF_TS_PRESENTATION      }, AMF_TS_PRESENTATION, AMF_TS_DECODE, VD, "timestamp_mode" },
    { NULL }
};

static const AVClass amf_decode_class = {
    .class_name = "amf",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVCodec ff_h264_amf_decoder = {
    .name           = "h264_amf",
//    .long_name      = NULL_IF_CONFIG_SMALL("AMD AMF decoder"),
    .long_name      = "AMD AMF decoder",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = AV_CODEC_ID_H264,
    .priv_data_size = sizeof(AvAmfDecoderContext),
    .priv_class     = &amf_decode_class,
    .init           = ff_amf_decode_init,
    .decode         = amf_decode_frame,
    .flush          = amf_decode_flush,
    .close          = ff_amf_decode_close,
    .pix_fmts       = ff_amf_pix_fmts,
    //.bsfs           = "h264", //TODO: real vcalue
    .capabilities   = AV_CODEC_CAP_HARDWARE | AV_CODEC_CAP_DELAY | //TODO: real vcalue
                      AV_CODEC_CAP_AVOID_PROBING,
    .wrapper_name   = "amf",
};

AVCodec ff_hevc_amf_decoder = {
    .name           = "hevc_amf",
//    .long_name      = NULL_IF_CONFIG_SMALL("AMD AMF decoder"),
    .long_name      = "AMD AMF decoder",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = AV_CODEC_ID_HEVC,
    .priv_data_size = sizeof(AvAmfDecoderContext),
    .priv_class     = &amf_decode_class,
    .init           = ff_amf_decode_init,
    .decode         = amf_decode_frame,
    .flush          = amf_decode_flush,
    .close          = ff_amf_decode_close,
    .pix_fmts       = ff_amf_pix_fmts,
    //.bsfs           = "h264", //TODO: real vcalue
    .capabilities   = AV_CODEC_CAP_HARDWARE | AV_CODEC_CAP_DELAY | //TODO: real vcalue
                      AV_CODEC_CAP_AVOID_PROBING,
    .wrapper_name   = "amf",
};
