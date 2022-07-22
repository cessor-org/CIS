#include "launcher.h"
#include <QDebug>

Launcher::Launcher(QObject *parent)
    : QObject{parent}
{
    //processor->setProcessChannelMode(QProcess::ProcessChannelMode::ForwardedChannels);
}
Launcher::~Launcher()
{
    qDebug() << "Debug" << "launcher" << "EOS";
}
void Launcher::launch ()
{
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("RELX_REPLACE_OS_VARS", "true");
    env.insert("COOKIE", "cookie_test_01");
    env.insert("PIN", "pin_test_00");
    env.insert("PORT", "31415");

    //QString cessor_path = "../../../_build/prod/rel/cessor/bin/cessor";
    QString cessor_path = "../../../bin/cessor";
    QStringList arguments;
    arguments << "console";

    processor = new QProcess(this);
    processor->setProcessEnvironment(env);
    processor->setProgram(cessor_path);
    processor->setArguments(arguments);

    connect(
            processor
            , SIGNAL(stateChanged(QProcess::ProcessState))
            , this
            , SLOT(onStateChanged(QProcess::ProcessState)));

    connect(
            processor
            , SIGNAL(started())
            , this
            , SLOT(onStarted()));

    connect(
            processor
            , SIGNAL(readyReadStandardOutput())
            , this
            , SLOT(onOutput()));

    connect(
            processor
            , SIGNAL(readyReadStandardError())
            , this
            , SLOT(onError()));

    connect(
            processor
            , SIGNAL(finished(int,QProcess::ExitStatus))
            , this
            , SLOT(onFinished(int,QProcess::ExitStatus)));

    connect(
            processor
            , SIGNAL(errorOccurred(QProcess::ProcessError))
            , this
            , SLOT(onErrorOccurred(QProcess::ProcessError)));

    processor->start();
/*
    CSTP_cb_t cb;
    cb.status = 0; //const int OK_STATUS = 0;
    emit callBack(cb);
*/
}
void Launcher::onStateChanged(QProcess::ProcessState state)
{
    qDebug() << "Debug" << "launcher" << "stateChanged"
             << "state:" << "state";
}
void Launcher::onStarted()
{
    qDebug() << "Debug" << "launcher" << "onStarted";
}
void Launcher::onOutput()
{

    qDebug() << "Debug" << "launcher"
             << "onOutput" << processor->readAllStandardOutput();

}
void Launcher::onError()
{
    qDebug() << "Debug" << "launcher" << "onError";
}
void Launcher::onFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    qDebug() << "Debug" << "launcher" << "onFinished"
             << "exitCode" << exitCode
             <<"exitStatus" << exitStatus;
}
void Launcher::onErrorOccurred(QProcess::ProcessError error)
{
    qDebug() << "Debug" << "launcher" << "errorOccurred"
             << "error" << error;
}
