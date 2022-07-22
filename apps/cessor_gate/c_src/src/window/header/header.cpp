#include "header.h"
#include <QDebug>

Header::Header(QWidget *parent)
{
    setObjectName("header_object");

    QRect rect;
    rect.setX(0);
    rect.setY(0);
    rect.setWidth(720);
    rect.setHeight(44);
    setGeometry(rect);

    setMinimumHeight(44);
    setMaximumHeight(44);

    QHBoxLayout *mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(5,5,5,5);

    //  Window controller
    control = new WindowControl;
    //connect(control, SIGNAL(minimize()), this, SIGNAL(minimize()));

    //  Tab bar
    tabBar = new TabBar;

    //  Decoration of elements
    QString os_type = QSysInfo::productType();
    if (os_type == "osx")
    {
        mainLayout->addWidget(control);
        mainLayout->addWidget(tabBar);
    }
    else {
        mainLayout->addWidget(tabBar);
        mainLayout->addWidget(control);
    }

    setLayout(mainLayout);

    setMouseTracking(true);
    //setAttribute(Qt::WA_TransparentForMouseEvents);

    QFile file(":/ui/style/header/header.css");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }

}
Header::~Header()
{

}
void Header::paintEvent (QPaintEvent *)
{
    QPainter painter(this);

    QStyleOptionFocusRect option;
    option.initFrom(this);
    //option.backgroundColor = palette().color(QPalette::Background);

    style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}

void Header::register_callBack(QWidget* parent)
{
    int x =10;
    tabBar->updateDimensions();
    //emit callback(x);
}

void Header::mouseDoubleClickEvent(QMouseEvent *e)
{
    qDebug() << "Header" <<  "mouseDoubleClickEvent";
    emit maximize();
}
void Header::upgradeMargins(QMargins margins)
{
    //qDebug() << "margins" << margins.right();
    if (margins.right() == 0 && margins.left() == 0)
    {
        QObject::setObjectName("header_no_radius");
        //style()->unpolish(this);
        style()->polish(this);
    }
    else if (margins.right() == 0)
    {
        QObject::setObjectName("header_right_no_radius");
        //style()->unpolish(this);
        style()->polish(this);
    }
    else if (margins.left() == 0)
    {
        QObject::setObjectName("header_left_no_radius");
        //style()->unpolish(this);
        style()->polish(this);
    }
    else
    {
        QObject::setObjectName("header_object");
        //style()->unpolish(this);
        style()->polish(this);
    }
}

