#include <QApplication>
#include <QWindow>
#include <iostream>
#include <QObject>

#include "window/mainwindow.h"

/** extern "C" =>  Remove C++ name mangling and makes the function 
 * compatible with C. It enforces C linkage to the annotated symbol. 
 **-----------------------------------------------------------------*/
#define EXPORT_C extern "C"

//  Application

EXPORT_C
QApplication* qt_app_new()
{
    std::cout << " [CPP] qt_app_new" << std::endl;
    static int   argc = 1;
    static const char* argv [] = { "Cessor Gate" };
    return new QApplication(argc, (char**) argv);
}

EXPORT_C
int qt_app_exec(QApplication* self)
{
    return self->exec();
}

//  Window

EXPORT_C
MainWindow* create_main_window()
{
    std::cout << " [CPP] create_main_window" << std::endl;
    return new MainWindow(nullptr);
}
EXPORT_C
void show_main_window(MainWindow* mWin)
{
    std::cout << " [CPP] show_main_window" << std::endl;
    mWin->showWindow();
    /*
    QWindow *window = mWin->windowHandle();
    if (window->startSystemResize(Qt::RightEdge | Qt::TopEdge))
        std::cout << " [CPP] x: OK" <<std::endl;
    window->reportContentOrientationChange(Qt::LandscapeOrientation);
    if(window->startSystemMove())
    std::cout << " [CPP] y: OK" << std::endl;
    */
}
EXPORT_C
void register_callback (
        MainWindow* mWin,
        void (* callback)(int x)
        )
{
    QObject::connect(
                mWin,
                //SIGNAL(callback(int)),
                qOverload<int>(&MainWindow::callback),
                [=](int x){
                    callback(x);
                }
    );
    mWin->set_callback();
}
typedef void   queue_t;
EXPORT_C
void register_callback_thread (
        MainWindow* mWin,
        queue_t* queue,
        void (* callback)(queue_t* queue, int x)
        )
{

    QObject::connect(
                mWin,
                //SIGNAL(callback(int)),
                qOverload<int>(&MainWindow::callback),
                [=](int x){
                    callback(queue, x);
                }
    );
        //std::cout << " [CPP] Debug" << std::endl;
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
    mWin->set_callback();
}
