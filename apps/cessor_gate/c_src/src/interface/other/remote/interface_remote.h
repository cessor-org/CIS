#ifndef INTERFACEREMOTE_H
#define INTERFACEREMOTE_H

#include <QObject>
#include <QFile>
#include <QProcess>
#include <QTimer>
#include <QDir>

#include "interface_state.h"
#include "cstp.h"
#include "crypto.h"
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
/*******************************/
/*  InterfaceRemote Synapses  */
/*****************************/
private:
    void key_gen();
    void launch_cessor();
    void state_file();
    void task_man(CSTP_cb_task_t);

public slots:
    void cstp_callBack(CSTP_cb_t);

private slots:
    void on_launcher_finished();

};

#endif // INTERFACEREMOTE_H
