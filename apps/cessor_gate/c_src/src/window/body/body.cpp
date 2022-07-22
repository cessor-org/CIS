#include "body.h"
Body::Body(QWidget *parent) : QWidget(parent)
{
    setObjectName("winBody");
    QRect rect(0, 44, 720, 600);
    setGeometry(rect);
    setMinimumHeight(600);
    mainLayout = new QHBoxLayout();
    mainLayout->setContentsMargins(0,0,0,0);

    view = new QWebEngineView();

    new_tab("cpanel");

    mainLayout->addWidget(view);
    setLayout(mainLayout);

}
Body::~Body()
{
}
void Body::updateView()
{
    TabPage* tabPage = tabPages.at(activePage_index);
    view->setPage(tabPage->page);
}

void Body::new_tab(QString appName)
{
    TabPage * tabPage = new TabPage;
    tabPage->setUp(appName);
    connect(tabPage, SIGNAL(toParent(QString)), this, SLOT(fromTabPage(QString)));

    activePage_index = tabPages.length();
    tabPages.insert(activePage_index, tabPage);
}
void Body::fromTabPage(QString request)
{
    if (request == "pageLoaded")
    {
        updateView();
    }
    else if (request == "loadError")
    {

    }
}
