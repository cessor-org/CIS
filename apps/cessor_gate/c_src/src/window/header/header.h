#ifndef HEADER_H
#define HEADER_H

#include <QWidget>
#include <QHBoxLayout>

#include <QPainter>
#include <QStyleOption>

#include <QStyle>

#include <QFile>
#include <QMouseEvent>

#include "windowcontrol.h"
#include "tabbar.h"

class Header : public QWidget
{
    Q_OBJECT
public:
    explicit Header(QWidget *parent = nullptr);
    ~Header();

    void register_callBack(QWidget* parent);
    void upgradeMargins(QMargins margins);

    WindowControl * control;
    TabBar * tabBar;


private:
    void paintEvent (QPaintEvent *);


    //void mouseMoveEvent(QMouseEvent *e);
    void mouseDoubleClickEvent(QMouseEvent *e);

signals:
    //void minimize();
    void maximize();
    //void close();


};

#endif // HEADER_H
