#include "tabbar.h"
#include <QDebug>

TabBar::TabBar(QWidget *parent) : QWidget(parent)
{
    setObjectName("tabBar");

    setMinimumHeight(34);
    setMaximumHeight(34);

    mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(0,0,0,0);

    //  tab list
    tabList = new TabList();
    connect(tabList, &TabList::updateDimensions, this, &TabBar::updateDimensions);
    connect(tabList, SIGNAL(toParent(QString)), this, SLOT(formTabList(QString)));

    //  new tab
    newTab = new QPushButton;
    newTab->setObjectName("newTab");
    QSize button_size(45,height());
    QSize icon_size(17,17);
    QIcon new_tab_icon = QIcon(":/ui/images/plus_icon_light_white.png"); //smaller
    newTab->setIcon(new_tab_icon);
    newTab->setIconSize(icon_size);
    newTab->setMinimumSize(button_size);
    newTab->setMaximumSize(button_size);
    newTab->setMouseTracking(true);
    connect(newTab, &QPushButton::clicked, tabList, &TabList::newTabRequest);

    //  arrow_left
    arrow_left = new QPushButton;
    arrow_left->setIconSize(QSize(15,15));
    arrow_left->setMinimumSize(QSize(30,height()));
    arrow_left->setMaximumSize(QSize(30,height()));
    arrow_left->setMouseTracking(true);
    connect(arrow_left, &QPushButton::clicked, tabList, &TabList::scroll_left);

    //  arrow_right
    arrow_right = new QPushButton;
    arrow_right->setIconSize(QSize(15,15));
    arrow_right->setMinimumSize(QSize(30,height()));
    arrow_right->setMaximumSize(QSize(30,height()));
    arrow_right->setMouseTracking(true);
    connect(arrow_right, &QPushButton::clicked, tabList, &TabList::scroll_right);
    //  arrow_down tab list
    arrow_down = new QPushButton;
    arrow_down->setObjectName("arrow_down");
    QIcon cursor_down_icon = QIcon(":/ui/images/Arrow-Down-white.png"); //smaller
    arrow_down->setIcon(cursor_down_icon);
    arrow_down->setIconSize(QSize(13,16));
    arrow_down->setMinimumSize(QSize(30,height()));
    arrow_down->setMouseTracking(true);
    //connect(arrow_down, &QPushButton::clicked, tabList, &TabList::scroll_left);

    //  spacer_left
    spacer_left = new QSpacerItem(30,height(), QSizePolicy::Minimum);
    //  spacer_right
    spacer_right = new QSpacerItem(30,height(), QSizePolicy::Minimum);


    //  insert barObjects
    mainLayout->insertSpacerItem(0, spacer_left);
    mainLayout->insertWidget(1,arrow_left,0,Qt::AlignHCenter);
    mainLayout->insertWidget(2,tabList,0,Qt::AlignHCenter);
    mainLayout->insertWidget(3,arrow_right,0,Qt::AlignHCenter);
    mainLayout->insertWidget(4,newTab,0,Qt::AlignHCenter);
    mainLayout->insertWidget(5,arrow_down,0,Qt::AlignHCenter);
    mainLayout->insertSpacerItem(6, spacer_right);

    setLayout(mainLayout);

    setMouseTracking(true);

    QFile file(":/ui/style/header/tabBar.css");

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
    int spacer_left_width = spacer_left->geometry().width();

    int arrow_left_width = arrow_left->isVisible()? arrow_left->width() : 0;
    int arrow_right_width = arrow_right->isVisible()? arrow_right->width() : 0;
    int arrow_down_width = arrow_down->isVisible()? arrow_down->width() : 0;

    int available_width = width();
    int spacer_right_width=0;

    int barList_width =
        spacer_left_width +
        arrow_left_width +
        arrow_right_width +
        newTab->width() +
        arrow_down_width;
    /*
    qDebug() << "TabBar" <<  "barList_width" << barList_width
                <<  "tabList->listWidth" << tabList->listWidth
                <<  "width" << width();
    */
    if (barList_width + tabList->listWidth >= width())
    {
        arrow_left->setVisible(true);
        arrow_right->setVisible(true);
        arrow_down->setVisible(true);

        spacer_right->changeSize(30, height(), QSizePolicy::Minimum);

        available_width -=
                spacer_left_width +
                arrow_left->width() +
                arrow_right->width() +
                newTab->width() +
                arrow_down->width() +
                30; //spacer_right->geometry().width();

        tabList->scrollBar.isPassive = false;
        tabList->scrollBar.isActive = true;
        tabList->updateWidth(available_width);
    }
    else
    {
        arrow_left->setVisible(false);
        arrow_right->setVisible(false);
        arrow_down->setVisible(false);

        spacer_right->changeSize(30, height(), QSizePolicy::Minimum);

        available_width -=
                spacer_left_width +
                newTab->width();

        tabList->scrollBar.isActive = false;
        tabList->updateWidth(available_width-30);

        spacer_right_width = available_width - tabList->width();
        spacer_right->changeSize(spacer_right_width, height(), QSizePolicy::Minimum);
        if (available_width==30)
            tabList->scrollBar.isPassive = true;
        else
            tabList->scrollBar.isPassive = false;

    }
    /*
    qDebug() << "TabBar" <<  "barList_width" << barList_width
             <<  "width" << width()
             <<  "available_width" << available_width
             <<  "tabList->listWidth" << tabList->listWidth
             <<  "tabList->width" << tabList->width()
             << "spacer_right_width" << spacer_right_width;
    */

    mainLayout->update();
    update();

}

void TabBar::formTabList(QString request)
{
    if (request == "arrow_left_active")
    {
        arrow_left->setObjectName("arrowActive_l");
        QIcon arrow_left_icon = QIcon(":/ui/images/Arrow-Left-White-New.png"); //smaller
        arrow_left->setIcon(arrow_left_icon);
        arrow_left->update();
        style()->polish(arrow_left);
    }
    else if (request == "arrow_left_passive")
    {
        arrow_left->setObjectName("arrowPassive_l");
        QIcon arrow_left_icon = QIcon(":/ui/images/Arrow-Left-Dark-New.png"); //smaller
        arrow_left->setIcon(arrow_left_icon);
        arrow_left->update();
        style()->polish(arrow_left);
    }
    else if (request == "arrow_right_active")
    {
        arrow_right->setObjectName("arrowActive_r");
        QIcon arrow_right_icon = QIcon(":/ui/images/Arrow-Right-White-New.png"); //smaller
        arrow_right->setIcon(arrow_right_icon);
        arrow_right->update();
        style()->polish(arrow_right);
    }
    else if (request == "arrow_right_passive")
    {
        arrow_right->setObjectName("arrowPassive_r");
        QIcon arrow_right_icon = QIcon(":/ui/images/Arrow-Right-Dark-New.png"); //smaller
        arrow_right->setIcon(arrow_right_icon);
        arrow_right->update();
        style()->polish(arrow_right);
    }
}
