#ifndef TABBAR_H
#define TABBAR_H

#include <QWidget>

#include <QFile>

#include <QPainter>
#include <QStyleOption>

#include <QHBoxLayout>
#include <QPushButton>

#include <QFontDatabase>

#include "tablist.h"
class TabBar : public QWidget
{
    Q_OBJECT
public:
    explicit TabBar(QWidget *parent = nullptr);
    ~TabBar();

    //barObjects barObjects;

    void updateDimensions();

public slots:
    void formTabList(QString request);

private:
    void paintEvent (QPaintEvent *);

    QHBoxLayout * mainLayout;
    QSpacerItem * spacer;

    QSpacerItem * spacer_left;
    QPushButton * arrow_left;
    TabList *     tabList;
    QPushButton * arrow_right;
    QPushButton * newTab;
    QPushButton * arrow_down;
    QSpacerItem * spacer_right;

    //void resizeEvent(QResizeEvent *event);

signals:

};

#endif // TABBAR_H
