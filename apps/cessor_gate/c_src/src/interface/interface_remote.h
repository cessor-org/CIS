#ifndef INTERFACEREMOTE_H
#define INTERFACEREMOTE_H

#include <QObject>
#include <QProcess>
#include <QTimer>

#include "interface_state.h"
#include "cstp/cstp.h"
#include "crypto/crypto.h"
#include "io/io.h"
#include "profile/profile.h"

//#include "cnode.h"

class InterfaceRemote : public QObject
{
    Q_OBJECT
public:
    explicit InterfaceRemote(QObject *parent = nullptr);
    ~InterfaceRemote();

    //void launch(int argc, char *argv[]);
    void init(int, char *[]);

private:
    STATE_t state;
    CSTP * cstp;
    Crypto * crypto;
    Profile * profile;
private:
    //ColdState * cold_state;
private:
    void async_requests();
    void async_cstx();
    void key_gen();

public slots:
    void cstp_callBack(CSTP_cb_t);
    void coldState_changed(QByteArray);

private slots:
    //void on_launcher_finished();

};

#endif // INTERFACEREMOTE_H
