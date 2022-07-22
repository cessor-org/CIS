#include "interface_remote.h"

/*************/
/*  PUBLIC  */
/***********/

InterfaceRemote::InterfaceRemote(QObject *parent) : QObject(parent)
{
    state.system = STATUS_UNSET;
    state.remote = STATUS_UNSET;

    state.launcher = new Launcher(this);
    connect(state.launcher, SIGNAL(callBack(CSTP_cb_t)), this, SLOT(cstp_callBack(CSTP_cb_t)));

    cstp = new CSTP(this);
    connect(cstp, SIGNAL(callBack(CSTP_cb_t)), this, SLOT(cstp_callBack(CSTP_cb_t)));

    crypto = new Crypto;

    ColdState * cold_state = new ColdState(this);
    connect(cold_state, SIGNAL(callBack(QByteArray)), this, SLOT(coldState_changed(QByteArray)));
}
InterfaceRemote::~InterfaceRemote()
{
    qDebug() << "Debug" << "InterfaceRemote" << "EOS";
}
void InterfaceRemote::init(int argc, char *argv[])
{

    CSTX_t cstx;
    cstx.CBI=0;cstx.VSD=0;cstx.VSN=0;   //  Version
    cstx.SD=0;cstx.SSD=0;               //  Service
    cstx.pRange=3;cstx.pPack=3;         //  Procedure
    //  State process module
    //  INIT process reference
    cstx.pMod=0;cstx.pRef=0;            //  Process

    cstx.parameters.clear();

    if (argc == 2)
    {
        QString command = QString::fromStdString(argv[1]);
        const int IS_EQUAl = 0;

        //  CLI
        if(QString::compare(command, "cli", Qt::CaseInsensitive) == IS_EQUAl)
        {
            state.remoteType.cli=true;
            state.async_requests.append(cstx);
            async_requests();
            return;
        }
        //  GUI
        if(QString::compare(command, "gui", Qt::CaseInsensitive) == IS_EQUAl)
        {
            state.remoteType.gui=true;
            state.async_requests.append(cstx);
            async_requests();
            return;

        }
    }
    //  Help
    state.remoteType.help=true;
    state.async_requests.append(cstx);
    async_requests();
}
/**************/
/*  PRIVATE  */
/************/

/****************************/
/*  Asynchronous Computing */
/**************************/
void InterfaceRemote::async_requests()
{
    if (state.async_requests.isEmpty())
    {
       async_cstx();
       return;
    }
    CSTX_t order = state.async_requests.first();
    qDebug() << "Debug" << "async_requests, order:" << order.pMod << order.pRef;
    state.async_requests.removeFirst();
    QByteArray CSTX;
    CSTP_compose_t cstx = cstp->compose(&order, &CSTX);
    if ( cstx.status == ERR_STATUS)
    {
        qDebug() << "Debug" << "async_requests" << "ERR_STATUS" << cstx.err;
        return;
    }
    if ( cstx.status == WARN_STATUS)
    {
        qDebug() << "Debug" << "async_requests" << "WARN_STATUS" << cstx.err;
        return;
    }
    cstp->compute(&CSTX, &state);
}
void InterfaceRemote::async_cstx()
{
    qDebug() << "Debug" << "async_cstx" << state.async_cstx.isEmpty();
    if (state.async_cstx.isEmpty())
    {
        //QTimer::singleShot(1000, this, &InterfaceRemote::async_cstx);
        //return;
        //state.launcher->launch();
        return;
    }
    QByteArray CSTX = state.async_cstx.first();
    state.async_cstx.removeFirst();
    cstp->compute(&CSTX, &state);
}

/*******************/
/*  CSTP Callback */
/*****************/
void InterfaceRemote::cstp_callBack(CSTP_cb_t cb)
{
    //  Status OK
    if (cb.status == OK_STATUS)
    {
        async_requests();
    }
    //  Status Warning
    else if (cb.status == WARN_STATUS)
    {
        qDebug() << "Debug" << "cstp_callBack" << "status" << cb.status << "error:" << cb.warn;
    }
    //  Status ERROR
    else // if (cb.status == ERR_STATUS)
    {
        qDebug() << "Debug" << "cstp_callBack" << "status" << cb.status << "error:" << cb.error;
    }
}

/****************/
/*  Cold State */
/**************/
void InterfaceRemote::coldState_changed(QByteArray CSTX)
{
    qDebug() << "Debug" << "coldState_changed";
    const int CALL_pMod = 1;
    const int STATUS_pRef = 0;
    CSTP_cb_t cb = cstp->permission(&CSTX, CALL_pMod, STATUS_pRef);
    if (cb.status == ERR_STATUS)
    {
        qDebug() << "Debug" << "coldState_changed" << "error:" << cb.error;
        return;
    }
    state.async_cstx.append(CSTX);
    InterfaceRemote::async_cstx();
}
