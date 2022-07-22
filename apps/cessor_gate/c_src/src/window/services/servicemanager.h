#ifndef SERVICEMANAGER_H
#define SERVICEMANAGER_H

#include <QObject>
#include <QSettings>
#include "webservice.h"

class ServiceManager : public QObject
{
    Q_OBJECT
public:
    explicit ServiceManager(QObject *parent = nullptr);
    ~ServiceManager();

    void new_service(QString serviceName);

private:
    QByteArray default_setting_document();

signals:

};

#endif // SERVICEMANAGER_H
