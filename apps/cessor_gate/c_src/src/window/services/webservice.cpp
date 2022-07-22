#include "webservice.h"
#include <QDebug>


WebService::WebService(QObject *parent) : QObject(parent)
{

}
WebService::~WebService()
{

}


bool WebService::setUp(QString serviceName)
{
    QDir bodyPath = QDir().absolutePath();
    bodyPath.cd("..");
    bodyPath.cd("APPs");
    bodyPath.cd(serviceName);

    QString appPath = bodyPath.path();
    appPath.append("/cessor_config.json");

    QFile file(appPath);

    QByteArray byteArray;
    QJsonParseError parseError;
    QJsonDocument jsonDoc;

    if (file.open(QIODevice::ReadOnly))
    {
        byteArray = file.readAll();
        file.close();

        jsonDoc = QJsonDocument::fromJson(byteArray, &parseError);
        if(parseError.error == QJsonParseError::NoError)
        {
            //qDebug() << "WebService" << "jsonDoc" << jsonDoc;
            return true;
        }
        return false;
    }
    return false;
}
