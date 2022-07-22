#ifndef WINDOWCONTROL_H
#define WINDOWCONTROL_H

#include <QWidget>

#include <QFile>

#include <QPainter>
#include <QStyleOption>

#include <QHBoxLayout>
#include <QPushButton>

class WindowControl : public QWidget
{
    Q_OBJECT
public:
    explicit WindowControl(QWidget *parent = nullptr);
    ~WindowControl();

private:
    void paintEvent (QPaintEvent *);

    void minimizeBtnClicked();
    void maximizeBtnClicked();
    void closeBtnClicked();

signals:
    void minimize();
    void maximize();
    void onClose();



};

#endif // WINDOWCONTROL_H
