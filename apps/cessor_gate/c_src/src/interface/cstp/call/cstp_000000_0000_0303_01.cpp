#include "cstp_000000_0000_0303_01.h"

CSTP_000000_0000_0303_01::CSTP_000000_0000_0303_01(QObject *parent)
    : QObject{parent}
{

}

CSTP_compose_t CSTP_000000_0000_0303_01::compose(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Processor Reference
    int pRef = order->pRef;
    //  INIT Processor Reference
    if (pRef == 0)
    {
        CSTX->append((char)pRef);
        //CSTX->append(order->parameters);
        //qDebug() << "Debug" << "INIT Processor Reference" << order->parameters.length();
        cb.status = OK_STATUS;
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.err.append("pRef");
    return cb;
}
CSTP_cb_t CSTP_000000_0000_0303_01::compute(QByteArray * CSTX, STATE_t * state)
{
    if (CSTX->length()>0)
    {
        // Processor Reference
        int pRef = CSTX->at(0);
        //  Interface Status Processor
        if (pRef == 0)
        {
            CSTX->remove(0,1);
            return processor_ref0(CSTX, state);
        }
        CSTP_cb_t cb;
        cb.status = ERR_STATUS;
        cb.error.append("pRef");
        return cb;
    }
    CSTP_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.append("length");
    return cb;

}
/**************/
/*  PRIVATE  */
/************/

/*******************/
/**  Permissions **/
/*****************/

CSTP_cb_t CSTP_000000_0000_0303_01::permission(QByteArray * CSTX, int pMod, int pRef)
{
    CSTP_cb_t cb;
    if (CSTX->length()<8)
    {
        cb.status = ERR_STATUS;
        cb.error.append("length");
        return cb;
    }
    // Version Crown-Block Index
    int VCBI = CSTX->at(0);
    // Version Serial Domain
    int VSD = CSTX->at(1);
    // Version Serial Number
    int VSN = CSTX->at(2);
    // Service Domain
    int SD = CSTX->at(3);
    // Service Sub-Domain
    int SSD = CSTX->at(4);
    // Procedure Range
    int pRange = CSTX->at(5);
    // Procedure Package
    int pPack = CSTX->at(6);
    if (    VCBI==0
            && VSD==0 && VSN==0
            && SD == 0 && SSD == 0
            && pRange == 3 && pPack == 3
            && pMod == CSTX->at(7)
            && pRef == CSTX->at(8))
    {
        cb.status = OK_STATUS;
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.error.append("permission");
    return cb;
}
int CSTP_000000_0000_0303_01::validation(QByteArray * CSTX)
{
    if (CSTX->length()<7)
    {
        return false;
    }

    time_t now;
    time(&now);  /* get current time; same as: now = time(NULL)  */

    struct tm timeStamp;
    //tm * timeStamp = new tm;
    timeStamp = *localtime(&now);

    //  Year
    int year1 = (int)(unsigned char)CSTX->at(0);
    int year2 = (int)(unsigned char)CSTX->at(1);
    int year = year1 * 256 + year2;
    timeStamp.tm_year = year-1900;

    //  Month
    int mon = (int)(unsigned char)CSTX->at(2);//1-12
    timeStamp.tm_mon = mon-1;//0-11

    //  Day
    int day = (int)(unsigned char)CSTX->at(3);
    timeStamp.tm_mday = day;

    //  Hour
    int hour = (int)(unsigned char)CSTX->at(4);
    timeStamp.tm_hour = hour;

    //  Minute
    int min = (int)(unsigned char)CSTX->at(5);
    timeStamp.tm_min = min;

    //  Second
    int sec = (int)(unsigned char)CSTX->at(6);
    timeStamp.tm_sec = sec;

    double seconds = difftime(now, mktime(&timeStamp));

    if (seconds<0)
    {
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "invalid <0"<<seconds;
        return ERR_STATUS;
    }
    if (seconds>180)    // Base Block period
    {
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "invalid >180"<<seconds;
        return ERR_STATUS;
    }
    CSTX->remove(0,7);
    qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "valid"<<seconds;
    return OK_STATUS;
}

/***********************/
/**  CALL Processors **/
/*********************/

/*****************************/
/*  System STATUS Processor */
/***************************/
CSTP_cb_t CSTP_000000_0000_0303_01::processor_ref0(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;
    if (CSTX->length()!=1)
    {
        cb.status = ERR_STATUS;
        cb.error.append("length");
        return cb;
    }
    int status = CSTX->at(0);
    if (status == STATUS_UNSET)
    {
        if (state->remote == STATUS_INIT)
        {
            //  BOOT request
            CSTX_t cstx;
            cstx.CBI=0;cstx.VSD=0;cstx.VSN=0;   //  Version
            cstx.SD=0;cstx.SSD=0;               //  Service
            cstx.pRange=3;cstx.pPack=3;         //  Procedure
            cstx.pMod=0;cstx.pRef=1;            //  Process

            state->async_requests.append(cstx);
            cb.status = OK_STATUS;
            return cb;
        }
    }
    if (status == STATUS_BOOT)
    {
        qDebug() << "Debug" << "Cessor BOOT";
    }
    cb.status = ERR_STATUS;
    cb.error.append("CSTP_000000_0000_0303_01");
    cb.error.append("processor_ref0");
    cb.error.append("status");
    return cb;
}
