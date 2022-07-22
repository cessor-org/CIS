#ifndef WEBSERVICE_H
#define WEBSERVICE_H

#include <QObject>
#include <QFile>
#include <QDir>
#include <QJsonDocument>
//#include <QJsonArray>

class WebService : public QObject
{
    Q_OBJECT
public:
    explicit WebService(QObject *parent = nullptr);
    ~WebService();

    bool setUp(QString serviceName);

signals:

};

#endif // WEBSERVICE_H
