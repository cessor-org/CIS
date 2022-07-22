#include "windowcontrol.h"
#include <QDebug>

WindowControl::WindowControl(QWidget *parent) : QWidget(parent)
{

    setObjectName("WindowControl");

    setMaximumHeight(30);
    setMinimumWidth(90);
    setMaximumWidth(90);

    QHBoxLayout *mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(0,0,0,0);

    //  Icons of buttons
    QIcon mini_btn_icon;
    QIcon maxi_btn_icon;
    QIcon close_btn_icon;

    QSize button_size(30,30);
    QSize icon_size(13,13);

    QString os_type = QSysInfo::productType();
    if (os_type == "osx")
    {

    }
    else {
        mini_btn_icon = QIcon(":/ui/images/minimize_icon_ligth_white.png");
        maxi_btn_icon = QIcon(":/ui/images/maximize_icon_ligth_white.png"); // should be smaller
        close_btn_icon = QIcon(":/ui/images/close_icon_ligth_white.png");
    }

    QPushButton *mini_btn = new QPushButton;
    mini_btn->setObjectName("mini_btn");
    mini_btn->setMaximumSize(button_size);
    mini_btn->setIcon(mini_btn_icon);
    mini_btn->setIconSize(icon_size);
    mini_btn->setMouseTracking(true);
    connect(mini_btn, &QPushButton::clicked, this, &WindowControl::minimizeBtnClicked);

    QPushButton *maxi_btn = new QPushButton;
    maxi_btn->setObjectName("maxi_btn");
    maxi_btn->setMaximumSize(button_size);
    maxi_btn->setIcon(maxi_btn_icon);
    maxi_btn->setIconSize(icon_size);
    maxi_btn->setMouseTracking(true);
    connect(maxi_btn, &QPushButton::clicked, this, &WindowControl::maximizeBtnClicked);

    QPushButton *close_btn = new QPushButton;
    close_btn->setObjectName("close_btn");
    close_btn->setMaximumSize(button_size);
    close_btn->setIcon(close_btn_icon);
    close_btn->setIconSize(icon_size);
    close_btn->setMouseTracking(true);
    connect(close_btn, &QPushButton::clicked, this, &WindowControl::closeBtnClicked);

    mainLayout->addWidget(mini_btn);
    mainLayout->addWidget(maxi_btn);
    mainLayout->addWidget(close_btn);
    setLayout(mainLayout);
    setMouseTracking(true);

    QFile file(":/ui/style/header/controlBar.css");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }
}
WindowControl::~WindowControl()
{

}
void WindowControl::paintEvent (QPaintEvent *)
{
    QPainter painter(this);

    QStyleOptionFocusRect option;
    option.initFrom(this);

    style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}
void WindowControl::minimizeBtnClicked()
{
    qDebug() << "minimizeBtnClicked";
    emit minimize();
}
void WindowControl::maximizeBtnClicked()
{
    qDebug() << "maximizeBtnClicked";
    emit maximize();
}
void WindowControl::closeBtnClicked()
{
    qDebug() << "closeBtnClicked";
    emit onClose();
}
