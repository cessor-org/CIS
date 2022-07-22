#include "tabpage.h"
TabPage::TabPage(QWidget *parent) : QWidget(parent)
{
    profile = new QWebEngineProfile();
    page = new QWebEnginePage(profile, this);
}
TabPage::~TabPage()
{
    profile->~QWebEngineProfile();
    page->~QWebEnginePage();
}
void TabPage::setUp(QString appName)
{
    qDebug() << "TabPage" << "setUp"
             << "appName" << appName;

    setObjectName(appName);

    QWebEngineSettings* setting = profile->settings();
    setting->setAttribute(QWebEngineSettings::PluginsEnabled, true);
    setting->setAttribute(QWebEngineSettings::XSSAuditingEnabled, false);
    setting->setAttribute(QWebEngineSettings::LocalContentCanAccessRemoteUrls, true);
    setting->setUnknownUrlSchemePolicy(QWebEngineSettings::AllowAllUnknownUrlSchemes);
    setting->setAttribute(QWebEngineSettings::DnsPrefetchEnabled, true);

    QDir bodyPath = QDir().absolutePath();
    qDebug() << "******" << "appPath test" << bodyPath.path();
    //bodyPath.cd("cessorUnit");
    bodyPath.cd("services");
    bodyPath.cd("webServices");
    bodyPath.cd(appName);
    bodyPath.cd("dist");

    QString appPath = bodyPath.path();
    appPath.append("/index.html");

    

    interceptor = new UrlRequestInterceptor(profile);
    interceptor->setUp(bodyPath.path());
    profile->setUrlRequestInterceptor(interceptor);
    connect(interceptor, SIGNAL(toParent(QString)), this, SLOT(frominterceptor(QString)));

    QUrl app_url = QUrl::fromLocalFile(appPath);
    page->load(app_url);

    //QUrl url = QUrl::fromUserInput("https://www.google.com");
    //page->load(url);
    connect(page, &QWebEnginePage::loadFinished, this, &TabPage::pageLoaded);
}

void TabPage::pageLoaded(bool loadState)
{
    if (loadState)
    {
        emit toParent("pageLoaded");

        QFile file(":/ui/script/cessorDriver.js");
        if (file.open(QFile::ReadOnly))
        {
            QString driver = file.readAll();
            page->runJavaScript(driver, [this](const QVariant &v)
            {
                //qDebug() << "start listening: " << v;
                //this->listenAgent ();
            });
            file.close();
        }
        /*
        page->runJavaScript("document.body.cessor;", [this](const QVariant &v)
        {
            this->processMSG(v);
        });
        */
    }
    else
    {
        emit toParent("loadError");
    }

}
void TabPage::listenAgent () {
    if (listenState)
    {
        page->runJavaScript("document.body.cessor.getReq;", [this](const QVariant &v)
        {
            this->processMSG(v);
        });
    }
}
void TabPage::processMSG(const QVariant &msg)
{
    QMap<QString, QVariant> map = msg.toMap();
    if (map.isEmpty())
    {
        qDebug() << "processMSG: " << msg << map.isEmpty();
        //page->runJavaScript("document.body.cessor.setRes={testKey: 1};");
    }
    else
    {
        QString value = map["test_request"].toString();
        qDebug() << "from app map | value:" << map << profile->scripts();

        QString key("testKey");
        QString js_code("document.body.cessor.setRes={");
        js_code.append(key);
        js_code.append(":'");
        js_code.append(value);
        js_code.append("'};");
        page->runJavaScript(js_code);

        page->runJavaScript("document.body.cessor.getVer;", [this](const QVariant &v)
        {
            this->processMSG(v);
        });

        qDebug() << "from app map | value:" << value << js_code;

        /*
        bool moveWindow = map["moveWindow"].toBool();
        if (moveWindow)
            qDebug() << "moveWindow x: " << map["x"].toInt() << "y: " << map["y"].toInt();
        */
        listenAgent ();
    }
}

void TabPage::frominterceptor(QString request)
{
    if (request == "LOADREQUEST")
    {
        //qDebug() << "TabPage"<<"frominterceptor"<<"LOADREQUEST";
        listenAgent();
    }
}
