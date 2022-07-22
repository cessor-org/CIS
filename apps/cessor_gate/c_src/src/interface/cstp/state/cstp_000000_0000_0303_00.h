#ifndef CSTP_000000_0000_0303_00_H
#define CSTP_000000_0000_0303_00_H

#include <QObject>
#include "../../profile/profile.h"

#include "../../interface_state.h"
#include "../coldstate.h"

class CSTP_000000_0000_0303_00 : public QObject
{
    Q_OBJECT
public:
    explicit CSTP_000000_0000_0303_00(QObject *parent = nullptr);
    ~CSTP_000000_0000_0303_00();

    CSTP_compose_t compose(CSTX_t * order, QByteArray * CSTX);
    CSTP_cb compute(QByteArray *, STATE_t*);

    CSTP_cb_t permission(QByteArray * CSTX,int pMod, int pRef);
private:
    /*  References */
    //  INIT Processor
    CSTP_cb processor_ref0(QByteArray *, STATE_t*);
    //  BOOT Processor
    CSTP_cb processor_ref1(QByteArray *, STATE_t*);
    //  STATUS Processor
    CSTP_cb processor_ref2(QByteArray *, STATE_t*);
signals:

};

#endif // CSTP_000000_0000_0303_00_H
