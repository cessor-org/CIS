#ifndef WINDOWPOSITION_H
#define WINDOWPOSITION_H

#include <QWidget>
#include <QDebug>
#include <QMouseEvent>
#include <QGuiApplication>
#include <QScreen>

typedef struct
{
    bool isActive;
    bool top;
    bool right;
    bool left;
    bool bottom;
} OnResize;

class WindowPosition : public QWidget
{
    Q_OBJECT
public:
    explicit WindowPosition(QWidget *parent = nullptr);
    ~WindowPosition();
    void init();
    void resizeEvent(QRect rect);
    void moveEvent(QRect rect);

    int MinimumWidth;
    int MinimumHeight;

public:
    //  Mouse events
    void mousePressEvent(QMouseEvent *e);
    void mouseReleaseEvent(QMouseEvent *e);
    void mouseDoubleClickEvent(QMouseEvent *e);
    void mouseMoveEvent(QMouseEvent *e);

private:
    QRect screen;
    QRect win_pos;

    QPoint oldPosition;

    QCursor cursor;

    QMargins margins;
    void check_margins();

    OnResize onResize;
    bool onMove;

    bool isMaximized;

signals:
    void upgradeGeometry(QRect rect);
    void upgradeMargins(QMargins margins);
    void updateCursor(QCursor cursor);
    void updateWidowState(Qt::WindowState state);

public slots:
    void minimize();
    void maximize();


};

#endif // WINDOWPOSITION_H
