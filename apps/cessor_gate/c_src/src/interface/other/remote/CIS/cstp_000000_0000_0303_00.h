#ifndef CSTP_000000_0000_0303_00_H
#define CSTP_000000_0000_0303_00_H

#include <QObject>
#include <QDebug>

#include "../interface_state.h"

class CSTP_000000_0000_0303_00 : public QObject
{
    Q_OBJECT
public:
    explicit CSTP_000000_0000_0303_00(QObject *parent = nullptr);
    ~CSTP_000000_0000_0303_00();

    void compose();
    CSTP_cb compute(QByteArray *, STATE_t*);
private:
    /*  References */
    //  Interface Status Processor Reference
    CSTP_cb processor_ref0(QByteArray *, STATE_t*);
signals:

};

#endif // CSTP_000000_0000_0303_00_H
