#include "urlrequestinterceptor.h"
#include <QLatin1String>
UrlRequestInterceptor::UrlRequestInterceptor(QObject *parent)
    :QWebEngineUrlRequestInterceptor(parent)
{
}
UrlRequestInterceptor::~UrlRequestInterceptor()
{

}
void UrlRequestInterceptor::setUp(QString appPath)
{
   workPath = appPath;
   workPath.prepend("file://");
   workPath.append("/");
}
void UrlRequestInterceptor::interceptRequest(QWebEngineUrlRequestInfo &info)
{
    QString request_url = info.requestUrl().toString();

    qDebug() << "UrlRequestInterceptor" << workPath << request_url;

    if(workPath.length() < request_url.length())
    {
        bool isMatch = true;
        int index =0;
        while (index < workPath.length())
        {
            if (workPath.at(index) != request_url.at(index))
            {
                isMatch = false;
                index = workPath.length();
            }
            else
                index++;
        }
        if (isMatch)
        {
            if (info.resourceType() == 13) // XMLHttpRequest
            {
                QString loadRequestPattern("LOADREQUEST");
                bool loadRequest = true;
                loadRequestPattern.prepend(workPath);
                if (loadRequestPattern == request_url)
                {
                    emit toParent("LOADREQUEST");
                }
                else
                {
                    info.block(true);
                }
            }
        }
        else
        {
            info.block(true);
        }
    }
    else
    {
        info.block(true);
    }
    /*
    QString rsrct = "";
    //QUrl url_local = QUrl::fromUserInput("http://localhost:3000/");
    switch(info.resourceType())
    {
        case 0:rsrct="ResourceTypeMainFrame = 0, // top level page";
        if (info.requestUrl() == url_local){
                info.redirect(QUrl("https://www.google.com"));
        }
        info.block(true);
        break;
        case 1:rsrct="ResourceTypeSubFrame, // frame or iframe";break;
        case 2:rsrct="ResourceTypeStylesheet, // a CSS stylesheet";break;
        case 3:rsrct="ResourceTypeScript, // an external script";break;
        case 4:rsrct="ResourceTypeImage, // an image (jpg/gif/png/etc)";break;
        case 5:rsrct="ResourceTypeFontResource, // a font";break;
        case 6:rsrct="ResourceTypeSubResource, // an other subresource.";break;
        case 7:rsrct="ResourceTypeObject, // an object (or embed) tag for a plugin,";break;
        case 8:rsrct="ResourceTypeMedia, // a media resource.";break;
        case 9:rsrct="ResourceTypeWorker, // the main resource of a dedicated worker.";break;
        case 10:rsrct="ResourceTypeSharedWorker, // the main resource of a shared worker.";break;
        case 11:rsrct="ResourceTypePrefetch, // an explicitly requested prefetch";break;
        case 12:rsrct="ResourceTypeFavicon, // a favicon";break;
        case 13:rsrct="ResourceTypeXhr, // a XMLHttpRequest"; break;
        case 14:rsrct="ResourceTypePing, // a ping request for <a ping>";break;
        case 15:rsrct="ResourceTypeServiceWorker, // the main resource of a service worker.";break;
        case 16:rsrct="ResourceTypeUnknown";break;

        default : rsrct="Unknown type";break;
    }
    //info.block(true);

      qDebug()<<"WebUrlRequestInterceptor::interceptRequest    " <<info.requestMethod()
             <<"\r\n  "<<info.requestUrl()<<"   "<<rsrct      <<"\r\n";

    */

}
