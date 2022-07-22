#include "tabmanager.h"

#include<QDebug>

TabManager::TabManager(QWidget *parent) : tabBar(parent)
{
    parent->setMaximumWidth(150);
}
TabManager::~TabManager(){
}
void TabManager::newTabBar(QString title){
    tabFrame(title);
}

void TabManager::tabFrame(QString title) {
    QFrame *tabFrame;
    tabFrame= new QFrame(tabBar);
    tabFrame->setMouseTracking(true);
    tabFrame->setObjectName("tab_frame");
    //tabFrame->setGeometry(15, 5, 153, 36);
    tabFrame->setGeometry(10, 4, 130, 36);
    tabIcon(tabFrame);
    tabTitle(tabFrame, title);
    //tabClose(tabFrame);
}
void TabManager::tabIcon(QFrame *tabFrame) {
    QPushButton *tab_icon;
    tab_icon= new QPushButton(tabFrame);
    tab_icon->setMouseTracking(true);
    tab_icon->setObjectName("tab_icon");
    tab_icon->setGeometry(5, 7, 22, 22);
    QString url = "./APPs/cessorPanel/icon.png";
    QIcon tabIcon = QIcon(url);
    tab_icon->setIcon(tabIcon);
    tab_icon->setIconSize(QSize(22, 22));
}
void TabManager::tabTitle(QFrame *tabFrame, QString title) {
    QLabel *tab_title;
    tab_title= new QLabel(tabFrame);
    tab_title->setMouseTracking(true);
    tab_title->setObjectName("tab_title");
    tab_title->setText( title );
    tab_title->setGeometry(31, 5, 100, 26);
}
void TabManager::tabClose(QFrame *tabFrame) {
    QPushButton *close_tab;
    close_tab= new QPushButton(tabFrame);
    close_tab->setMouseTracking(true);
    close_tab->setObjectName("tab_close");
    close_tab->setGeometry(130, 8, 20, 20);
    QIcon tabClose = QIcon(":/ui/images/close_tab.png");
    close_tab->setIcon(tabClose);
    close_tab->setIconSize(QSize(12, 12));
    qDebug() << "************debuger";
}
void TabManager::setGeo(int newWidth) {
    qDebug() << "newWidth : " << newWidth;
}
