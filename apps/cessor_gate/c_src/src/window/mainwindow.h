#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QApplication>
#include <QMainWindow>
#include <QScreen>
#include <QFile>
#include <QFormLayout>
#include <QRect>
#include <QVBoxLayout>//dep
#include "windowposition.h"
//#include "window.h"
#include "windowagent.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    enum class TitleMode { CleanTitle = 0, OnlyCloseButton, MaxMinOff, FullScreenMode, MaximizeModeOff, MinimizeModeOff, FullTitle };
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

    void showWindow();
    void set_callback( );
protected:
    void setGeometry0(int ax, int ay, int aw, int ah);

signals:
    void callback (int x);

private:
   // WindowAgent * window;
    WindowAgent * win_agent;
    WindowPosition * win_pos;

/*
 *
 *   Window position manager
 *
 */
private:
    void setUp();
    void mousePressEvent(QMouseEvent *e);
    void mouseReleaseEvent(QMouseEvent *e);
    void mouseDoubleClickEvent(QMouseEvent *e);
    void mouseMoveEvent(QMouseEvent *e);
    void resizeEvent(QResizeEvent *event);
    void moveEvent(QMoveEvent *event);


public slots:
    void upgradeGeometry(QRect rect);
    void upgradeMargins(QMargins margins);
    void updateCursor(QCursor cursor);
    void updateWidowState(Qt::WindowState state);
    void onClose();


};

#endif // MAINWINDOW_H



/*
 *  MainWindow
 *      window_position
 *      Window
 *          header
 *          body
 *
 *
 */
