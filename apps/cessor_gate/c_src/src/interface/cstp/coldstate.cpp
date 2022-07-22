#include "coldstate.h"
ColdState::ColdState(QObject *parent)
    : QObject{parent}
{
    watcher = new QFileSystemWatcher(this);
    watcher->addPath("../../../services/system/gate/state/INTERFACE");
    connect(watcher, SIGNAL(fileChanged(QString)), this, SLOT(coldState_changed(QString)));
}
int ColdState::read(QByteArray *CSTX)
{
    CSTP_cb_t cb;

    //  The state file of interface has to be found on
    //  "cessor/services/system/gate/state/INTERFACE"

    QString interfacePort;
    interfacePort.clear();
    interfacePort.append("../../../services/system/gate/state/INTERFACE");

    QFile file(interfacePort);
    if (!file.open(QIODevice::ReadOnly))
    {
        qDebug() << "Debug" << "cold_state, open file problem";
        return ERR_STATUS;
    }
    QByteArray FILE = file.readAll();

    CSTX->clear();
    for (int i=0;i<FILE.size();i++)
        CSTX->insert(i,FILE.at(i));

    file.close();
    return OK_STATUS;
}
void ColdState::coldState_changed(QString change)
{
    QByteArray CSTX;
    if (read(&CSTX) == ERR_STATUS)
        return;
    emit callBack(CSTX);

}
