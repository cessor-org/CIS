#ifndef LAUNCHER_H
#define LAUNCHER_H

#include <QObject>
#include <QProcess>
#include "../cstp/cstx.h"
#include "../cstp/cstp_callback.h"

class Launcher : public QObject
{
    Q_OBJECT
public:
    explicit Launcher(QObject *parent = nullptr);
    ~Launcher();
    void launch ();
private:
    QProcess * processor;
private slots:
    void onStateChanged(QProcess::ProcessState);
    void onOutput();
    void onError();
    void onStarted();
    void onFinished(int exitCode, QProcess::ExitStatus exitStatus);
    void onErrorOccurred(QProcess::ProcessError error);
signals:
    void callBack(CSTP_cb_t);
};

#endif // LAUNCHER_H
