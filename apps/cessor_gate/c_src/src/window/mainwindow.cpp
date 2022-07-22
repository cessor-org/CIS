#include "mainwindow.h"
//#include "ui_mainwindow.h"
//#include <QWindow>
#include <QDebug>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    win_agent = new WindowAgent;

    win_pos = new WindowPosition;

    connect(win_agent->header->control, SIGNAL(minimize()), win_pos, SLOT(minimize()));
    connect(win_agent->header->control, SIGNAL(maximize()), win_pos, SLOT(maximize()));
    connect(win_agent->header, SIGNAL(maximize()), win_pos, SLOT(maximize()));
    connect(win_agent, SIGNAL(onClose()), this, SLOT(onClose()));
    connect(win_pos, SIGNAL(upgradeGeometry(QRect)), this, SLOT(upgradeGeometry(QRect)));
    connect(win_pos, SIGNAL(upgradeMargins(QMargins)), this, SLOT(upgradeMargins(QMargins)));
    connect(win_pos, SIGNAL(updateCursor(QCursor)), this, SLOT(updateCursor(QCursor)));
    connect(win_pos, SIGNAL(updateWidowState(Qt::WindowState)), this, SLOT(updateWidowState(Qt::WindowState)));

    win_pos->init();

    setCentralWidget(win_agent);
    setUp();

    qDebug() << "QSysInfo::productType()" << QSysInfo::productType();


}

MainWindow::~MainWindow()
{
    win_agent->~WindowAgent();
    win_pos->~WindowPosition();
}
// ---------- Wrapper API
void MainWindow::set_callback( )
{
   //qDebug() << "[MainWindow] set_callback";
    int x =113;
    emit callback(x);
}

void MainWindow::setUp()
{
    setWindowFlags(Qt::Window);
    setWindowFlags(Qt::CustomizeWindowHint);
    setWindowFlags(Qt::FramelessWindowHint);
    setMouseTracking(true);

    setAttribute(Qt::WA_DeleteOnClose);
    setAttribute(Qt::WA_TranslucentBackground);

    QFile file(":/ui/style/mainWindow.css");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }

    setMinimumHeight(win_pos->MinimumHeight);
    setMinimumWidth(win_pos->MinimumWidth);

    QApplication::setWindowIcon(QIcon(":/ui/images/cessor_icons/gray_light.png"));
    //QApplication::setAttribute(Qt::AA_EnableHighDpiScaling); //must be set before QCoreApplication is created

    QCoreApplication::setOrganizationName("Cessor Project");
    QCoreApplication::setApplicationName("Cessor Gtae");
    //QCoreApplication::setAttribute(Qt::AA_UseOpenGLES); //must be set before QCoreApplication is created

}
void MainWindow::showWindow()
{
    qDebug() << "MainWindow" << "showWindow";
    this->show();
    win_agent->header->tabBar->updateDimensions();

}
/*
 *
 *   Window position manager
 *
 */
void MainWindow::resizeEvent(QResizeEvent *event)
{
    //qDebug() << "MainWindow" << "resizeEvent" << event ;
    win_pos->resizeEvent(geometry());
    win_agent->header->tabBar->updateDimensions();

}
void MainWindow::moveEvent(QMoveEvent *event)
{
    QRect rect = geometry();
    //qDebug() << "MainWindow" << "moveEvent" << event << rect << x();
    win_pos->moveEvent(rect);
}

void MainWindow::mousePressEvent(QMouseEvent *e)
{
    qDebug() << "MainWindow " << "mousePressEvent";
    win_pos->mousePressEvent(e);
}
void MainWindow::mouseReleaseEvent(QMouseEvent *e)
{
    qDebug() << "MainWindow " <<  "mouseReleaseEvent";
    win_pos->mouseReleaseEvent(e);
}
void MainWindow::mouseDoubleClickEvent(QMouseEvent *e)
{
    qDebug() << "MainWindow " <<  "mouseDoubleClickEvent";
    win_pos->mouseDoubleClickEvent(e);
}
void MainWindow::mouseMoveEvent(QMouseEvent *e)
{
    //qDebug() << "MainWindow " << "mouseMoveEvent" << e;
    win_pos->mouseMoveEvent(e);
}
//  public slots
void MainWindow::upgradeGeometry(QRect rect)
{
    setGeometry(rect);
}
void MainWindow::upgradeMargins(QMargins margins)
{
    setContentsMargins(margins);
    win_agent->header->upgradeMargins(margins);
}
void MainWindow::updateCursor(QCursor cursor)
{
    //qDebug() << "cursor " << cursor;
    setCursor(cursor);
}
void MainWindow::updateWidowState(Qt::WindowState state)
{
    setWindowState(state);
}
void MainWindow::onClose()
{
    qDebug() << "MainWindow" << "onClose" ;
    close();
}
