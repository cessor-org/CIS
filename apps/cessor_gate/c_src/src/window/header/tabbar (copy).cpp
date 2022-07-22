#include "tabbar.h"
#include <QDebug>

TabBar::TabBar(QWidget *parent) : QWidget(parent)
{
    setObjectName("tabBar");

    setMinimumWidth(100);

    mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(0,0,0,0);

    //  main page
    QPushButton *mainPage = new QPushButton;
    mainPage->setObjectName("mainPage");
    mainPage->setText("C§");
    //mainPage->setContentsMargins(0,0,0,0);
    mainPage->setMinimumWidth(60);
    mainPage->setMaximumWidth(60);

    //  new page
    QPushButton *newPage = new QPushButton;
    newPage->setObjectName("newPage");
    QSize button_size(45,34);
    QSize icon_size(17,17);
    QIcon new_tab_icon = QIcon(":/ui/images/plus_icon_white.png"); //smaller
    newPage->setIcon(new_tab_icon);
    newPage->setIconSize(icon_size);
    newPage->setMinimumSize(button_size);
    newPage->setMaximumSize(button_size);

    //  Spacer
    spacer = new QSpacerItem(0,height(), QSizePolicy::Minimum);

    mainLayout->insertWidget(0,mainPage,0,Qt::AlignHCenter);
    mainLayout->insertWidget(1,newPage,0,Qt::AlignHCenter);

    //mainLayout->addWidget(mainPage);
    //mainLayout->addWidget(newPage);
    mainLayout->addSpacerItem(spacer);

    //mainLayout->insertWidget()


    setLayout(mainLayout);

    QFile file(":/ui/style/header-tabBar.css");

    QFontDatabase::addApplicationFont(":/ui/style/font/blinker.ttf");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }
}
TabBar::~TabBar()
{

}

void TabBar::paintEvent (QPaintEvent *)
{
    QPainter painter(this);

    QStyleOptionFocusRect option;
    option.initFrom(this);

    style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}
void TabBar::updateDimensions()
{
    QList<QPushButton *> tab_list = findChildren<QPushButton *>();
    int tabsWidth = 0;
    foreach(QPushButton * tab, tab_list)
    {
        tabsWidth += tab->width();
    }
    int spaceWidth = width() - tabsWidth;
    spacer->changeSize(spaceWidth, height(), QSizePolicy::Minimum);
    mainLayout->update();

    qDebug() << "TabBar" <<  geometry() << tabsWidth <<
                spaceWidth << spacer->geometry();

}
