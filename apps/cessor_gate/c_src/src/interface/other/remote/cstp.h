#ifndef CSTP_H
#define CSTP_H

#include <QObject>

#include "interface_state.h"
#include "CIS/cstp_000000_0000_0303_00.h"

class CSTP : public QObject
{
    Q_OBJECT
public:
    explicit CSTP(QObject *parent = nullptr);
    ~CSTP();

    void compose();
    bool permission(QByteArray *, QString);
    void compute(QByteArray *, STATE_t*);

private:
    void version(QByteArray *, STATE_t*);
    void service(QByteArray *, STATE_t*);
    void procedure(QByteArray *, STATE_t*);
    void process(QByteArray *, STATE_t*);

signals:
    void callBack(CSTP_cb_t);

};

#endif // CSTP_H
