#ifndef TABBAR_H
#define TABBAR_H

#include <QWidget>

#include <QFile>

#include <QPainter>
#include <QStyleOption>

#include <QHBoxLayout>
#include <QPushButton>

#include <QFontDatabase>

typedef struct
{
    QSpacerItem * spacer_left;
    QPushButton * cursor_left;
    QPushButton * mainTab;
    QSpacerItem * spacer_dynamic;
    QPushButton * cursor_right;
    QPushButton * newTab;
    QPushButton * dropDown;
    QSpacerItem * spacer_right;
} objects;

class TabBar : public QWidget
{
    Q_OBJECT
public:
    explicit TabBar(QWidget *parent = nullptr);
    ~TabBar();

    void updateDimensions();

private:
    void paintEvent (QPaintEvent *);

    QHBoxLayout *mainLayout;
    QSpacerItem * spacer;

signals:

};

#endif // TABBAR_H
