#ifndef URLREQUESTINTERCEPTOR_H
#define URLREQUESTINTERCEPTOR_H

#include <QObject>
#include <QtWebEngineWidgets>

class UrlRequestInterceptor : public QWebEngineUrlRequestInterceptor
{
    Q_OBJECT
public:
    explicit UrlRequestInterceptor(QObject *parent = nullptr);
    ~UrlRequestInterceptor();
    void setUp(QString appPath);

private:
    QString workPath;
    void interceptRequest(QWebEngineUrlRequestInfo &info);
signals:
    void toParent(QString request);

};

#endif // URLREQUESTINTERCEPTOR_H
