#ifndef WINDOWAGENT_H
#define WINDOWAGENT_H

#include <QWidget>
QT_FORWARD_DECLARE_CLASS(QVBoxLayout)

#include <QPainter>
#include <QStyleOption>

#include "header/header.h"
#include "body/body.h"
#include "services/servicemanager.h"
class WindowAgent : public QWidget
{
    Q_OBJECT
public:
    explicit WindowAgent(QWidget *parent = nullptr);
    ~WindowAgent();
    void setUp();

    Header * header;
    Body * body;

protected:


private:
    QWidget * window;
    //void mousePressEvent(QMouseEvent *e);
    void paintEvent (QPaintEvent *);


    //void maximize();
    //void close();

public slots:
    void onCloseEvent();

signals:
    void onClose();


};

#endif // WINDOWAGENT_H
