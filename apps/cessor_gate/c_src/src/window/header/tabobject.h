#ifndef TABOBJECT_H
#define TABOBJECT_H

#include <QWidget>
#include <QSizePolicy>

#include <QFile>

#include <QPainter>
#include <QStyleOption>

#include <QHBoxLayout>
#include <QPushButton>
#include <QLabel>

#include <QFontDatabase>

#include <QResizeEvent>


class TabObject : public QWidget
{
    Q_OBJECT
public:
    explicit TabObject(QWidget *parent = nullptr);
    ~TabObject();

    void setUp(QString objectName, QString tabName, QString iconUrl);

private:
    void paintEvent (QPaintEvent *);
    QHBoxLayout * mainLayout;
    QPushButton * iconTab;
    QLabel * titleTab;
    QPushButton * closeTab;

    void mousePressEvent(QMouseEvent *e);
    void resizeEvent(QResizeEvent *event);
    void focusInEvent(QFocusEvent *event);
    void focusOutEvent(QFocusEvent *event);

    void closeTabClicked();

signals:
    void toParent(QString request, QWidget * this_widgrt);

};

#endif // TABOBJECT_H
