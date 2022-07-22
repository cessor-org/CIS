/*
    %%%-------------------------------------------------------------------
    %% @doc
    %%  Cessor Information Systems, CIS
    %%  CIS License v0.1.0
    %%  https://cessor.org
    %%  Cessor Service Transaction Protocol
    %%  Version Crown-Block index = 0
    %%  Version Serial Domain = 0
    %%  Version Serial Number = 0
    %%  Service Domain = 0;         System service domain
    %%  Service Sub-Domain = 0;     System service sub-domain
    %%  Procedure Range = 3;        Gate Procedure Packages Range
    %%  Procedure Package = 3;      Interface_remote Procedure Package
    %%  Process Module = 0;         State Process Module
    %% @end
    %%%-------------------------------------------------------------------
 */
#include <time.h>

#include "cstp_000000_0000_0303_00.h"

#include <QFile>

CSTP_000000_0000_0303_00::CSTP_000000_0000_0303_00(QObject *parent) : QObject(parent)
{

}
CSTP_000000_0000_0303_00::~CSTP_000000_0000_0303_00()
{

}
CSTP_compose_t CSTP_000000_0000_0303_00::compose(CSTX_t * order, QByteArray * CSTX)
{
    //  Set callback
    CSTP_compose cb;

    // Processor Reference
    int pRef = order->pRef;
    //  Interface Status Processor Reference
    if (pRef == 0)
    {
        CSTX->append((char)pRef);
        CSTX->append(order->parameters);
        cb.status = OK_STATUS;
        return cb;
    }
    //  Interface Status Processor Reference
    if (pRef == 1)
    {
        CSTX->append((char)pRef);
        cb.status = OK_STATUS;
        return cb;
    }
    cb.status = ERR_STATUS;
    cb.err.append("pRef");
    return cb;
}
CSTP_cb CSTP_000000_0000_0303_00::compute(QByteArray * CSTX, STATE_t * state)
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
        //  Launch Cessor Processor
        if (pRef == 1)
        {
            CSTX->remove(0,1);
            return processor_ref1(CSTX, state);
        }
        CSTP_cb cb;
        cb.status = ERR_STATUS;
        cb.error.append("pRef");
        return cb;
    }
    CSTP_cb cb;
    cb.status = ERR_STATUS;
    cb.error.append("length");
    return cb;

}

/**************/
/*  PRIVATE  */
/************/

//  FT
CSTP_cb ft(FT_t FT)
{
    CSTP_cb cb;
    cb.status = ERR_STATUS;
    cb.error.insert(0, FT.type);
    cb.error.insert(1, "profile");
    cb.error.insert(2, FT.issuer);

    for(int index=0; FT.fault.length(); index++)
    {
        QString fault = FT.fault.at(index);
        cb.error.insert(3+index, fault);
    }
    return cb;
}

/*******************/
/**  Permissions **/
/*****************/

CSTP_cb_t CSTP_000000_0000_0303_00::permission(QByteArray * CSTX, int pMod, int pRef)
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

/************************/
/**  State Processors **/
/**********************/

/********************/
/*  INIT Processor */
/******************/
CSTP_cb CSTP_000000_0000_0303_00::processor_ref0(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb_t cb;

    if (CSTX->length()!=0)
    {
        cb.status = ERR_STATUS;
        cb.error.append("length");
        return cb;
    }

    if (state->remoteType.help)
    {
        IO * io = new IO(this);
        io->print(PRINT_help);
        cb.status = OK_STATUS;
        return cb;
    }

    ColdState * cold_state = new ColdState(this);
    QByteArray CSTX_cold;
    if (cold_state->read(&CSTX_cold) == ERR_STATUS)
    {
        cb.status = ERR_STATUS;
        cb.error.append("bad_state_file");
        return cb;
    }

    const int STATE_pMod = 1;
    const int STATUS_pRef = 0;
    if (permission(&CSTX_cold, STATE_pMod, STATUS_pRef).status == ERR_STATUS)
    {
        cb.status = ERR_STATUS;
        cb.error.append("CSTP_000000_0000_0303_00");
        cb.error.append("processor_ref0");
        cb.error.append("permission");
        return cb;
    }

    if (state->remoteType.cli)
    {
        IO * io = new IO(this);
        io->print(PRINT_splash);
    }

    state->remote = STATUS_INIT;
    state->async_cstx.append(CSTX_cold);
    cb.status = OK_STATUS;
    return cb;
}

/********************/
/*  BOOT Processor */
/******************/
CSTP_cb CSTP_000000_0000_0303_00::processor_ref1(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb cb;
    FT_t FT;
    FT.issuer = "processor_ref1";

    if (CSTX->length()!=0)
    {
        FT.type = "error";
        FT.fault.append("length");
        return ft(FT);
    }

    //  Check if there is public key
    QString pub_key("../../../services/system/gate/crypto/PUBLIC");
    QFile file(pub_key);
    if (file.open(QIODevice::ReadOnly))
    {
        qDebug() << "Debug" << "There are public key";

        //  Task to login
        CSTP_cb_task_t task;
        task.name = "sign";
        task.args.append("login");

        cb.status = OK_STATUS;
        return cb;
    }
    /*
    //  Task to signup
    CSTP_cb_task_t task;
    task.name = "sign";
    task.args.append("signup");

    Profile * profile = new Profile(this);
    Profile_cb_t profile_cb = profile->task_man(task, state);
    if (profile_cb.status == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.append("profile");
        FT.fault.append(profile_cb.error);
        return ft(FT);
    }
    */
    state->launcher->launch();
    qDebug() << "Debug" << "Remote BOOT";
    state->remote = STATUS_BOOT;
    cb.status = OK_STATUS;
    return cb;
}

/**********************/
/*  STATUS Processor */
/********************/
CSTP_cb CSTP_000000_0000_0303_00::processor_ref2(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb cb;
    return cb;
}
