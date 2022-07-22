#ifndef TABMANAGER_H
#define TABMANAGER_H

#include <QWidget>
#include <QFrame>
#include <QPushButton>
#include <QLabel>

class TabManager : public QWidget
{
    Q_OBJECT
public:
    /**
     * @brief The TabFlag defines the type of TabBar that will be shown.
     */
    enum class TabFlag { HOME, APP };
    TabManager(QWidget *tabBar);
    ~TabManager();
    void newTabBar(QString title);
    void tabFrame(QString title);
    void tabIcon(QFrame *tabFrame);
    void tabTitle(QFrame *tabFrame, QString title);
    void tabClose(QFrame *tabFrame);
    void setGeo(int newWidth);

private:
    QWidget *tabBar;
};

#endif // TABMANAGER_H
