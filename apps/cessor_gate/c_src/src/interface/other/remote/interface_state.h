#ifndef INTERFACE_STATE_H
#define INTERFACE_STATE_H

#include <QObject>
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
typedef struct STATE
{
    int status;//0:init; 1:boot; 2:ready
    Remote_type_t remoteType;
}
STATE_t;
/*****************/
/*  Call Back   */
/***************/
typedef struct CSTP_cb_task
{
    QString name;// Task name
    int delay;//    Delay of task execution
}
CSTP_cb_task_t;

typedef struct CSTP_cb_error
{
    QString name;// Error name
}
CSTP_cb_error_t;

typedef struct CSTP_cb
{
    int status; //0:ok; 1:error;
    QList<CSTP_cb_task_t> tasks;//list of tasks
    QList<CSTP_cb_error> error;//list of errors
}
CSTP_cb_t;

const int OK_STATUS = 0;
const int ERR_STATUS = -1;

#endif // INTERFACE_STATE_H
