#ifndef CSTP_000000_0000_0303_01_H
#define CSTP_000000_0000_0303_01_H

#include <QObject>
#include <QTextCodec>
#include "../../interface_state.h"
#include "../coldstate.h"

class CSTP_000000_0000_0303_01 : public QObject
{
    Q_OBJECT
public:
    explicit CSTP_000000_0000_0303_01(QObject *parent = nullptr);

    CSTP_compose_t compose(CSTX_t * order, QByteArray * CSTX);
    CSTP_cb_t compute(QByteArray *, STATE_t*);

    CSTP_cb_t permission(QByteArray * CSTX,int pMod, int pRef);
private:
    int validation(QByteArray * CSTX);
private:
    /*  References */
    //  INIT Processor
    CSTP_cb_t processor_ref0(QByteArray *, STATE_t*);
    CSTP_cb_t processor_ref1(QByteArray *, STATE_t*);

signals:

};

#endif // CSTP_000000_0000_0303_01_H
