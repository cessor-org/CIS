#ifndef BODY_H
#define BODY_H

#include <QWidget>
#include <QHBoxLayout>
#include <QFile>

#include "tabpage.h"

class Body : public QWidget
{
    Q_OBJECT
public:
    explicit Body(QWidget *parent = nullptr);
    ~Body();

    void new_tab(QString appName);
    void updatePage();

public slots:
    void fromTabPage(QString request);

private:
    QHBoxLayout * mainLayout;

    QWebEngineView * view;

    QList<TabPage*> tabPages;
    int activePage_index;

    void updateView();


signals:

};

#endif // BODY_H
