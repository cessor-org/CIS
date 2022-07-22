#ifndef TABLIST_H
#define TABLIST_H

#include <QWidget>

#include <QFile>

#include <QPainter>
#include <QStyleOption>

#include <QHBoxLayout>
#include <QPushButton>

#include <QFontDatabase>
#include "tabobject.h"

typedef struct
{
    bool isActive;
    bool isPassive;

    int window_left;
    int window_right;

    int tabWidth;

    int activeTab_index;
    int passiveTab_index;
} ScrollBar;

class TabList : public QWidget
{
    Q_OBJECT
public:
    explicit TabList(QWidget *parent = nullptr);
    ~TabList();

    void newTabRequest();
    void updateWidth(int available_width);
    void scroll_left();
    void scroll_right();

    int listWidth;
    ScrollBar scrollBar;

public slots:
    void formChildren(QString request, QWidget *child);

private:
    void paintEvent (QPaintEvent *);
    QHBoxLayout * mainLayout;
    TabObject * mainTab;

    int MAXIMUM_TAB_WIDTH;
    int MINIMUM_TAB_WIDTH;

    void newTab(QString tabName, QString icon_url);

    void scalability();
    void visibility();
    void focus_update();


signals:
    void updateDimensions();
    void toParent(QString request);


};

#endif // TABLIST_H
