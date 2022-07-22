#ifndef CSTP_H
#define CSTP_H

#include <QObject>

#include "../interface_state.h"
#include "state/cstp_000000_0000_0303_00.h"
#include "call/cstp_000000_0000_0303_01.h"



class CSTP : public QObject
{
    Q_OBJECT
public:
    explicit CSTP(QObject *parent = nullptr);
    ~CSTP();

    CSTP_compose_t compose(CSTX_t * order, QByteArray * CSTX);
    void compute(QByteArray *, STATE_t*);

    CSTP_cb_t permission(QByteArray * CSTX, int pMod, int pRef);

    //  Compose
private:
    //ColdState * cold_state;

private:
    CSTP_compose_t compose_version(CSTX_t * order, QByteArray * CSTX);
    CSTP_compose_t compose_service(CSTX_t * order, QByteArray * CSTX);
    CSTP_compose_t compose_procedure(CSTX_t * order, QByteArray * CSTX);
    CSTP_compose_t compose_process(CSTX_t * order, QByteArray * CSTX);
    //  Compute
private:
    void compute_version(QByteArray *, STATE_t*);
    void compute_service(QByteArray *, STATE_t*);
    void compute_procedure(QByteArray *, STATE_t*);
    void compute_process(QByteArray *, STATE_t*);

signals:
    void callBack(CSTP_cb_t);

private slots:
    //void coldState_changed(QByteArray);
signals:
    //void callBack(QByteArray);
};

#endif // CSTP_H
