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

/*************/
/*  PUBLIC  */
/***********/

CSTP::CSTP(QObject *parent) : QObject(parent)
{
    //cold_state = new ColdState(this);
    //connect(cold_state, SIGNAL(callBack(QByteArray)), this, SLOT(coldState_changed(QByteArray)));
}
CSTP::~CSTP()
{

}
CSTP_compose_t CSTP::compose(CSTX_t * order, QByteArray * CSTX)
{
    return compose_version(order, CSTX);
}
void CSTP::compute(QByteArray * CSTX, STATE_t * state)
{
    if (CSTX->length()>8)
    {
        // Version Control
        compute_version(CSTX, state);
        return;
    }
    CSTP_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.append("pMod");
    emit callBack(cb);
}
CSTP_cb_t CSTP::permission(QByteArray * CSTX, int pMod, int pRef)
{
    if (pMod == 0)
    {
        CSTP_000000_0000_0303_00 * stateMod = new CSTP_000000_0000_0303_00;
        return stateMod->permission(CSTX,pMod,pRef);
    }
    if (pMod == 1)
    {
        CSTP_000000_0000_0303_01 * callMod = new CSTP_000000_0000_0303_01;
        return callMod->permission(CSTX,pMod,pRef);
    }
    CSTP_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.append("pMod");
    return cb;
}
/**************/
/*  PRIVATE  */
/************/

/**************/
/*  Compose  */
/************/

CSTP_compose_t CSTP::compose_version(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Version Crown-Block Index
    int CBI = order->CBI;
    // Version Serial Domain
    int VSD = order->VSD;
    // Version Serial Number
    int VSN = order->VSN;

    if (CBI == 0)
    {
        CSTX->append((char)0);
        if (VSD == 0)
        {
            CSTX->append((char)0);
            if (VSN == 0)
            {
                CSTX->append((char)0);
                // Service Control
                return compose_service(order, CSTX);
            }
            cb.status = ERR_STATUS;
            cb.err.append("VSN");
            return cb;
        }
        cb.status = ERR_STATUS;
        cb.err.append("VSD");
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.err.append("CBI");
    return cb;

}
CSTP_compose_t CSTP::compose_service(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Service Domain
    int SD = order->SD;
    // Service Sub-Domain
    int SSD = order->SSD;

    if (SD == 0)
    {
        CSTX->append((char)0);
        if (SSD == 0)
        {
            CSTX->append((char)0);
            // Procedure Control
            return compose_procedure(order, CSTX);
        }
        cb.status = ERR_STATUS;
        cb.err.append("SSD");
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.err.append("SD");
    return cb;

}
CSTP_compose_t CSTP::compose_procedure(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Procedure Range
    int pRange = order->pRange;
    // Procedure Package
    int pPack = order->pPack;

    //  Gate procedure range
    if (pRange == 3)
    {
        CSTX->append((char)pRange);
        //  Interface remote procedure package
        if (pPack == 3)
        {
            CSTX->append((char)pPack);
            // Process Control
            return compose_process(order, CSTX);
        }
        cb.status = ERR_STATUS;
        cb.err.append("pPack");
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.err.append("pRange");
    return cb;

}
CSTP_compose_t CSTP::compose_process(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Process Module
    int pMod = order->pMod;

    // State Process Module
    if (pMod == 0)
    {
        CSTX->append((char)pMod);
        // Processor Reference
        CSTP_000000_0000_0303_00 * pRef = new CSTP_000000_0000_0303_00;
        return pRef->compose(order, CSTX);
    }
    // Call Process Module
    else if (pMod == 1)
    {
        CSTX->append((char)pMod);
        // Processor Reference
        CSTP_000000_0000_0303_01 * pRef = new CSTP_000000_0000_0303_01;
        return pRef->compose(order, CSTX);
    }
    cb.status = ERR_STATUS;
    cb.err.append("pMod");
    return cb;

}

/**************/
/*  Compute  */
/************/

void CSTP::compute_version(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;
    // Version Crown-Block Index
    int CBI = CSTX->at(0);
    // Version Serial Domain
    int VSD = CSTX->at(1);
    // Version Serial Number
    int VSN = CSTX->at(2);

    if (CBI == 0)
    {
        if (VSD == 0)
        {
            if (VSN == 0)
            {
                // Service Control
                CSTX->remove(0,3);
                compute_service(CSTX, state);
                return;
            }
            cb.status = ERR_STATUS;
            cb.error.append("VSN");
            emit callBack(cb);
            return;
        }
        cb.status = ERR_STATUS;
        cb.error.append("VSD");
        emit callBack(cb);
        return;
    }
    cb.status = ERR_STATUS;
    cb.error.append("CBI");
    emit callBack(cb);
}
void CSTP::compute_service(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;
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
            compute_procedure(CSTX, state);
            return;
        }
        cb.status = ERR_STATUS;
        cb.error.append("SSD");
        emit callBack(cb);
        return;
    }
    cb.status = ERR_STATUS;
    cb.error.append("SD");
    emit callBack(cb);
}
void CSTP::compute_procedure(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;
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
            compute_process(CSTX, state);
            return;
        }
        cb.status = ERR_STATUS;
        cb.error.append("pPack");
        emit callBack(cb);
        return;
    }
    cb.status = ERR_STATUS;
    cb.error.append("pRange");
    emit callBack(cb);
}
void CSTP::compute_process(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;
    // Process Module
    int pMod = CSTX->at(0);

    // State Process Module
    if (pMod == 0)
    {
        // Processor Reference
        CSTX->remove(0,1);
        CSTP_000000_0000_0303_00 * pRef = new CSTP_000000_0000_0303_00;
        cb = pRef->compute(CSTX, state);
        emit callBack(cb);
        return;
    }
    // Call Process Module
    if (pMod == 1)
    {
        // Processor Reference
        CSTX->remove(0,1);
        CSTP_000000_0000_0303_01 * pRef = new CSTP_000000_0000_0303_01;
        cb = pRef->compute(CSTX, state);
        emit callBack(cb);
        return;
    }
    cb.status = ERR_STATUS;
    cb.error.append("pMod");
    emit callBack(cb);
}
