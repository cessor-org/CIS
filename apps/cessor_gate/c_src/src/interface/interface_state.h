#ifndef INTERFACE_STATE_H
#define INTERFACE_STATE_H

#include <QObject>
#include <QDebug>
#include "launcher/launcher.h"
#include "cstp/cstx.h"
#include "cstp/cstp_callback.h"
/*****************/
/*  Interface   */
/***************/
typedef struct Interface
{
    bool state;
    bool certificate;
    bool priv_key ;
}
Interface_t;
/*************/
/*  STATE   */
/***********/
typedef struct Remote_type
{
    bool cli;
    bool gui;
    bool help;
}
Remote_type_t;
typedef struct CRYPO
{
    QString secret;
}
CRYPO_t;

typedef struct STATE
{
    int system;//system status; status: 0:unset; 1:init; 2:boot; 3=ready
    int remote;//remote status; status: 0:unset; 1:init; 2:boot; 3=ready
    Remote_type_t remoteType;
    CRYPO_t crypto;
    QList<CSTX_t> async_requests;
    QList<QByteArray> async_cstx;
    Launcher * launcher;
}
STATE_t;
/*****************/
/*  Call Back   */
/***************/
typedef struct CSTP_cb_task
{
    QString name;// Task name
    int delay;//    Delay of task execution
    QList<QString> args;
}
CSTP_cb_task_t;

/************/
/*  CSTP   */
/**********/
typedef struct CSTP_compose
{
    int status;
    QByteArray cstx;    //?
    QList<QString> err;
}
CSTP_compose_t;

/*************/
/*  STATUS  */
/***********/

const int OK_STATUS = 0;
const int ERR_STATUS = -1;
const int WARN_STATUS = 1;

/// Machine statuses

const int STATUS_UNSET = 0;
const int STATUS_INIT = 1;
const int STATUS_BOOT = 2;
const int STATUS_READY = 3;


///     FT
typedef struct FT
{
    QString type;   //warn; error
    QString issuer;
    QList<QString> fault;

}
FT_t;


#endif // INTERFACE_STATE_H
