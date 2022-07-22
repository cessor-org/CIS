#include "windowagent.h"


WindowAgent::WindowAgent(QWidget *parent) : QWidget(parent)
{
    setObjectName("WindowAgent");

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(0,0,0,0);

    header = new Header;
    mainLayout->addWidget(header);

    connect(header->control, SIGNAL(onClose()), this, SLOT(onCloseEvent()));
    //header->register_callBack(WindowAgent);

    body = new Body;
    mainLayout->addWidget(body);

    ServiceManager * manager = new ServiceManager;


    setLayout(mainLayout);
    setMouseTracking(true);

    //body->setVisible(false);
    QFile file(":/ui/style/WindowAgent.css");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }
    setWindowState(Qt::WindowMaximized);
}
WindowAgent::~WindowAgent()
{

}
void WindowAgent::paintEvent (QPaintEvent *)
{
    QStyleOption opt;
    opt.init (this);

    QPainter p(this);
    style()->drawPrimitive (QStyle::PE_Widget, &opt, &p, this);
}
void WindowAgent::setUp ()
{
    QRect rect_main(0, 0, 720, 644);
    setGeometry(rect_main);
    setGeometry(0,0,720,644);
}

//  Mouse events
void WindowAgent::onCloseEvent()
{
    qDebug() << "WindowAgent " << "onCloseEvent";
    //if there are open tabs rage a warning message
    emit onClose();
}
