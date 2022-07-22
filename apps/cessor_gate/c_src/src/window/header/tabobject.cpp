#include "tabobject.h"
#include <QDebug>

TabObject::TabObject(QWidget *parent) : QWidget(parent)
{
    mainLayout = new QHBoxLayout();
    setLayout(mainLayout);

    setMouseTracking(true);

    QFile file(":/ui/style/header/tabObject.css");

    QFontDatabase::addApplicationFont(":/ui/style/font/blinker.ttf");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }
}
TabObject::~TabObject()
{

}
void TabObject::paintEvent (QPaintEvent *)
{
    QPainter painter(this);

    QStyleOptionFocusRect option;
    option.initFrom(this);

    style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}
void TabObject::setUp(QString objectName, QString tabName, QString iconUrl)
{
    if(objectName == "tabObject")
    {
        setObjectName(objectName);
        mainLayout->setSpacing(2);
        mainLayout->setContentsMargins(2,4,2,4);

        //  Icon of tab
        iconTab = new QPushButton;
        iconTab->setIconSize(QSize(20,20));
        iconTab->setMinimumWidth(20);
        iconTab->setMaximumWidth(20);
        QIcon tab_icon = QIcon(iconUrl);
        iconTab->setIcon(tab_icon);
        iconTab->setMouseTracking(true);

        //  Title of tab
        titleTab= new QLabel;
        titleTab->setText(tabName);
        titleTab->setMinimumWidth(37);
        titleTab->setMaximumWidth(147);
        titleTab->setMouseTracking(true);

        //  close button
        closeTab = new QPushButton;
        QIcon close_icon = QIcon(":/ui/images/close_icon_ligth_white.png");
        closeTab->setIcon(close_icon);
        closeTab->setIconSize(QSize(13,13));
        closeTab->setMinimumWidth(25);
        closeTab->setMinimumHeight(25);
        closeTab->setMaximumWidth(25);
        closeTab->setMouseTracking(true);
        connect(closeTab, &QPushButton::pressed, this, &TabObject::closeTabClicked);

        mainLayout->insertWidget(0,iconTab,0,Qt::AlignLeft);
        mainLayout->insertWidget(1,titleTab,0,Qt::AlignHCenter);
        mainLayout->insertWidget(2,closeTab,0,Qt::AlignRight);
    }
    else if(objectName == "mainTab")
    {
        qDebug() << "TabObject" << "mainTab"  ;
        setObjectName(objectName);
        mainLayout->setSpacing(0);
        //mainLayout->setContentsMargins(30,0,30,0);
        mainLayout->setContentsMargins(0,0,0,2);

        //  Title of tab
        titleTab= new QLabel;
        //titleTab->setObjectName("titleMain");
        titleTab->setText("C§");
        //titleTab->setMinimumWidth(35);
        //titleTab->setMaximumWidth(35);
        //titleTab->setMinimumHeight(34);

        mainLayout->insertWidget(0,titleTab,0,Qt::AlignHCenter);
        //mainLayout->addWidget(titleTab);

        setMinimumWidth(90);
        setMaximumWidth(90);


    }
    mainLayout->update();
    update();

}
void TabObject::resizeEvent(QResizeEvent *event)
{
    if(objectName() == "tabObject")
    {
        qDebug() << "TabObject" << "titleTab" << titleTab->geometry() ;
        if (hasFocus())
        {
            qDebug() << "TabObject" << "resizeEvent" << hasFocus() ;
            closeTab->setVisible(true);
        }
        else
        {
            if (event->size().width() < 110)
            {
                closeTab->setVisible(false);
                mainLayout->setSpacing(2);
            }
            else
            {
                closeTab->setVisible(true);
                int spacer = 2+(event->size().width() / 90 * 5);
                mainLayout->setSpacing(spacer);
                mainLayout->setContentsMargins(
                            spacer,
                            mainLayout->contentsMargins().top(),
                            spacer,
                            mainLayout->contentsMargins().bottom());
            }
        }
        update();
        int closeTabWidth =
                closeTab->isVisible()?
                    mainLayout->spacing() + closeTab->width() : 0;
        int titleWidth =
                width() -
                (
                    mainLayout->contentsMargins().left() +
                    iconTab->width() +
                    mainLayout->spacing() +
                    closeTabWidth
                 );
        titleTab->setMinimumWidth(titleWidth);
        update();
    }
}
void TabObject::focusInEvent(QFocusEvent *event)
{
    //qDebug() << "TabObject" << "focusInEvent" << event ;
    if(objectName() == "tabObject")
    {
        closeTab->setObjectName("closeTabActive");
        style()->polish(closeTab);
        titleTab->setObjectName("titleTabActive");
        style()->polish(titleTab);
        closeTab->setVisible(true);
        int closeTabWidth = mainLayout->spacing() + closeTab->width();
        int titleWidth =
                width() -
                (
                    mainLayout->contentsMargins().left() +
                    iconTab->width() +
                    mainLayout->spacing() +
                    closeTabWidth
                 );
        titleTab->setMinimumWidth(titleWidth);
    }
    else if(objectName() == "mainTab")
    {
        titleTab->setObjectName("titleMainActive");
        style()->polish(titleTab);
    }
    update();
}
void TabObject::focusOutEvent(QFocusEvent *event)
{
    if(objectName() == "tabObject")
    {
        closeTab->setObjectName("closeTabPassive");
        style()->polish(closeTab);
        titleTab->setObjectName("titleTabPassive");
        style()->polish(titleTab);
        closeTab->setVisible(false);
        if (width() > 90+15)
        {
            closeTab->setVisible(true);
        }
        int closeTabWidth =
                closeTab->isVisible()?
                    mainLayout->spacing() + closeTab->width() : 0;
        int titleWidth =
                width() -
                (
                    mainLayout->contentsMargins().left() +
                    iconTab->width() +
                    mainLayout->spacing() +
                    closeTabWidth
                 );
        titleTab->setMinimumWidth(titleWidth);
    }
    else if(objectName() == "mainTab")
    {
        titleTab->setObjectName("titleMainPassive");
        style()->polish(titleTab);
    }
    update();
    emit toParent("unFocus", this);
}
void TabObject::mousePressEvent(QMouseEvent *e)
{
    emit toParent("focus", this);

}
void TabObject::closeTabClicked()
{
    emit toParent("close", this);

}
