/*
 *
 *  Cessor Information Systems
 *  Cessor Service Transaction Protocol
 *
 *  Version Crown-Block index = 0
 *  Version Serial Domain = 0
 *  Version Serial number = 0
 *
 *  Service Domain = 0
 *  Service Sub-Domain = 0; System service
 *
 *  Procedure Range = 3;  Gate Packages Range
 *  Procedure Package = 3;  Interface Remote package
 *
 */

#include "cstp.h"

CSTP::CSTP(QObject *parent) : QObject(parent)
{
}
CSTP::~CSTP()
{

}
void CSTP::compose()
{

}
void CSTP::compute(QByteArray * CSTX, STATE_t * state)
{
    if (CSTX->length()>8)
    {
        // Version Control
        version(CSTX, state);
    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        cb.error.clear();
        emit callBack(cb);
        this->~CSTP();
    }
}
bool CSTP::permission(QByteArray * CSTX, QString caller)
{
    if (CSTX->length()<8)
    {
        return false;
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
    // Process Module
    int pMod = CSTX->at(7);
    qDebug() << "Debug" << "permission" << "0";
    if (    caller == "state"
            && VCBI==0
            && VSD==0
            && VSN==0
            && SD == 0
            && SSD == 0
            && pRange == 3
            && pPack == 3
            && pMod == 0)
        return true;
    else
        return false;
}
/**********/
/*  BIF  */
/********/

void CSTP::version(QByteArray * CSTX, STATE_t * state)
{
    // Version Crown-Block Index
    int VCBI = CSTX->at(0);
    // Version Serial Domain
    int VSD = CSTX->at(1);
    // Version Serial Number
    int VSN = CSTX->at(2);

    if (VCBI == 0)
    {
        if (VSD == 0)
        {
            if (VSN == 0)
            {
                // Service Control
                CSTX->remove(0,3);
                service(CSTX, state);
            }
            else
            {

            }
        }
        else
        {

        }
    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        cb.error.clear();
        emit callBack(cb);
        this->~CSTP();
    }
}
void CSTP::service(QByteArray * CSTX, STATE_t * state)
{
    // Service Domain
    int SD = CSTX->at(0);
    // Service Sub-Domain
    int SSD = CSTX->at(1);

    if (SD == 0)
    {
        if (SSD == 0)
        {
            // Procedure Control
            CSTX->remove(0,2);
            procedure(CSTX, state);
        }
        else
        {

        }
    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        cb.error.clear();
        emit callBack(cb);
        this->~CSTP();
    }
}
void CSTP::procedure(QByteArray * CSTX, STATE_t * state)
{
    // Procedure Range
    int pRange = CSTX->at(0);
    // Procedure Package
    int pPack = CSTX->at(1);

    //  Gate procedure range
    if (pRange == 3)
    {
        //  Interface remote procedure package
        if (pPack == 3)
        {
            // Process Control
            CSTX->remove(0,2);
            process(CSTX, state);
        }
        else
        {

        }
    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        cb.error.clear();
        emit callBack(cb);
        this->~CSTP();
    }
}
void CSTP::process(QByteArray * CSTX, STATE_t * state)
{
    // Process Module
    int pMod = CSTX->at(0);

    // State Process Module
    if (pMod == 0)
    {
        // Processor Reference
        CSTX->remove(0,1);
        CSTP_000000_0000_0303_00 * pRef = new CSTP_000000_0000_0303_00;
        CSTP_cb cb = pRef->compute(CSTX, state);
        if (cb.status == 0 )
        {
            pRef->~CSTP_000000_0000_0303_00();
            emit callBack(cb);
            //this->~CSTP();
        }

    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        cb.error.clear();
        emit callBack(cb);
        this->~CSTP();
    }
}
