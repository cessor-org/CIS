#include "servicemanager.h"
#include <QDebug>
#include <QJsonValue>
#include <QJsonArray>
#include <QJsonObject>
#include <QFileDevice>

ServiceManager::ServiceManager(QObject *parent) : QObject(parent)
{
    QFile file(":/ui/config/servicemanager.json");



    QByteArray byteArray;
    QJsonParseError parseError;
    QJsonDocument jsonDoc;

    if (file.open(QIODevice::ReadWrite))
    {
        byteArray = file.readAll();

        jsonDoc = QJsonDocument::fromJson(byteArray, &parseError);
        if(parseError.error == QJsonParseError::NoError)
        {
            QJsonObject jsonObj = jsonDoc.object();
            QJsonValue value = jsonObj["serviceList"];

            if (value.isArray())
            {
                QJsonArray array(value.toArray());
                for (int index=0 ; index< array.size() ; index++)
                {
                    QJsonValue element = array.at(index);
                    if (element.isString())
                    {
                        qDebug() << "ServiceManager" << "config" << element.toString();
                        QString serviceName = element.toString();
                        WebService * service = new WebService;
                        if (service->setUp(serviceName))
                        {
                            // insert in children list
                        }
                        else
                        {
                            service->~WebService();
                            array.removeAt(index);
                            index--;
                        }
                    }
                    else
                    {
                        array.removeAt(index);
                        index--;
                    }
                }
                jsonObj["serviceList"] = array;
                byteArray = QJsonDocument(jsonObj).toJson();
            }
            else
            {
                byteArray = default_setting_document();
            }
        }
        else
        {
            byteArray = default_setting_document();
        }

        file.write(byteArray);
        file.close();
    }
    else
    {
        //bool test = file.setPermissions(QFileDevice::Permissions permissions);
        //qDebug() << "ServiceManager" << "permissions" << file.permissions()<< test;
    }
}
ServiceManager::~ServiceManager()
{

}

void ServiceManager::new_service(QString serviceName)
{
    WebService * service = new WebService;
    if (service->setUp(serviceName))
    {
        // insert in children list
        // insert in setting_document
    }
    else
    {
        service->~WebService();
    }
}

QByteArray ServiceManager::default_setting_document()
{


    QJsonArray jsonArray;
    jsonArray.append("cpanel");

    QJsonObject jsonObj;
    jsonObj["serviceList"] = jsonArray;

    QByteArray byteArray;
    byteArray = QJsonDocument(jsonObj).toJson();

    return byteArray;

}
