#ifndef CSTP_CALLBACK_H
#define CSTP_CALLBACK_H
#include <QObject>

/*****************/
/*  Call Back   */
/***************/
typedef struct CSTP_cb
{
    int status;
    QList<QString> error;//list of error
    QList<QString> warn;//list of warning
}
CSTP_cb_t;

/*************/
/*  STATUS  */
/***********/
/*
const int OK_STATUS = 0;
const int ERR_STATUS = -1;
const int WARN_STATUS = 1;
*/
#endif // CSTP_CALLBACK_H
