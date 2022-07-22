#ifndef COLDSTATE_H
#define COLDSTATE_H

#include <QObject>
#include <QFile>
#include <QFileSystemWatcher>
#include "../interface_state.h"

#include <iostream>
#include <cstdint>
#include <cstring>

class ColdState : public QObject
{
    Q_OBJECT
public:
    explicit ColdState(QObject *parent = nullptr);

    int read(QByteArray *CSTX);
private:
    STATE_t state;

private:
    QFileSystemWatcher * watcher;

public slots:
    void coldState_changed(QString);
signals:
    void callBack(QByteArray);

};

#endif // COLDSTATE_H
