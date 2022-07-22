#ifndef TABPAGE_H
#define TABPAGE_H

#include <QWidget>

#include "urlrequestinterceptor.h"

class TabPage : public QWidget
{
    Q_OBJECT
public:
    explicit TabPage(QWidget *parent = nullptr);
    ~TabPage();

    QWebEnginePage *page;

    void setUp(QString appName);


public slots:
    void frominterceptor(QString request);
protected:


private:
    QWebEngineProfile * profile;
    UrlRequestInterceptor * interceptor;
    void pageLoaded(bool state);
    void listenAgent();
    bool listenState;
    void processMSG(const QVariant &msg);


signals:
    void toParent(QString request);

};

#endif // TABPAGE_H
