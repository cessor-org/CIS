#include "tablist.h"
#include <QDebug>

TabList::TabList(QWidget *parent) : QWidget(parent)
{
    setObjectName("tabList");

    MAXIMUM_TAB_WIDTH = 200;
    MINIMUM_TAB_WIDTH = 90;

    mainLayout = new QHBoxLayout();
    mainLayout->setSpacing(5);
    mainLayout->setContentsMargins(5,0,5,0);

    //  main tab
    mainTab = new TabObject;
    mainTab->setUp("mainTab", "", "");
    connect(mainTab, SIGNAL(toParent(QString,QWidget*)), this, SLOT(formChildren(QString,QWidget*)));



    scrollBar.tabWidth = MAXIMUM_TAB_WIDTH;
    listWidth = mainTab->width() +10;

    mainLayout->addWidget(mainTab);

    setLayout(mainLayout);

    scrollBar.isActive = false;
    scrollBar.activeTab_index = 0;
    scrollBar.passiveTab_index = 0;

    focus_update();
    setMouseTracking(true);

    QFile file(":/ui/style/header/tabList.css");

    if (file.open(QFile::ReadOnly))
    {
        setStyleSheet(file.readAll());
        file.close();
    }
}
TabList::~TabList()
{

}

void TabList::paintEvent (QPaintEvent *)
{
    QPainter painter(this);

    QStyleOptionFocusRect option;
    option.initFrom(this);

    style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}
//  private
void TabList::newTab(QString tabName, QString icon_url)
{
    TabObject * newTabObject = new TabObject;
    newTabObject->setUp("tabObject", tabName, icon_url);
    newTabObject->resize(scrollBar.tabWidth, height());

    connect(newTabObject, SIGNAL(toParent(QString,QWidget*)), this, SLOT(formChildren(QString,QWidget*)));

    mainLayout->addWidget(newTabObject);

    QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");
    scrollBar.activeTab_index = tab_list.length();

    mainLayout->update();
    update();

    if (scrollBar.isActive)
    {
        scrollBar.window_left = 2; //temporary
        scrollBar.window_right = scrollBar.activeTab_index+1;
        visibility();
        focus_update();
    }
    else if (scrollBar.isPassive)
    {
        listWidth =
                mainTab->width() +
                tab_list.length() * (scrollBar.tabWidth + mainLayout->spacing()) +
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();

        listWidth += MINIMUM_TAB_WIDTH;
        emit updateDimensions();
    }
    else
    {
        scalability();
        emit updateDimensions();
    }
}
void TabList::scalability()
{
    QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");

    if (tab_list.length() >0)
    {
        int available_width =
                maximumWidth() -
                mainTab->width() -
                (tab_list.length() * mainLayout->spacing());

        int pixel_per_tab = available_width / tab_list.length();

        if (pixel_per_tab > MAXIMUM_TAB_WIDTH)
            scrollBar.tabWidth = MAXIMUM_TAB_WIDTH;
        else if (pixel_per_tab <= MINIMUM_TAB_WIDTH)
            scrollBar.tabWidth = MINIMUM_TAB_WIDTH;
        else
            scrollBar.tabWidth = pixel_per_tab;

        mainTab->setVisible(true);
        foreach(QWidget * tab, tab_list)
        {
            tab->setVisible(true);
            tab->setMaximumWidth(scrollBar.tabWidth);
            tab->setMinimumWidth(scrollBar.tabWidth);
            tab->update();
        }
        listWidth =
                mainTab->width() +
                tab_list.length() * (scrollBar.tabWidth + mainLayout->spacing()) +
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();
    }
    else
    {
        listWidth =
                mainTab->width() +
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();
    }
    resize(listWidth,height());
    mainLayout->update();
    update();
}
void TabList::visibility()
{
    QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");

    int available_width = maximumWidth();
    int pixel_per_tab;
    int available_tabs;

    if (scrollBar.window_left == 0)
    {
        mainTab->setVisible(true);
        available_width -=
                mainTab->width() +
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();

        available_tabs = (int) available_width / MINIMUM_TAB_WIDTH;

        scrollBar.window_right = available_tabs+1;
    }
    else if (scrollBar.window_left == 1)
    {
        mainTab->setVisible(false);


        available_width -=
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();

        available_tabs = (int) available_width / MINIMUM_TAB_WIDTH;
    }
    else
    {
        mainTab->setVisible(false);
        available_width -=
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();

        available_tabs = (int) available_width / MINIMUM_TAB_WIDTH;

        scrollBar.window_left = scrollBar.window_right - available_tabs;
    }

    pixel_per_tab =
            (int)(
                available_width -
                available_tabs * MINIMUM_TAB_WIDTH
             ) / available_tabs
            + MINIMUM_TAB_WIDTH;

    scrollBar.tabWidth = pixel_per_tab;
    listWidth = (tab_list.length()+1) * MINIMUM_TAB_WIDTH;

    for (int index=0; index<tab_list.length(); index++) {
        if (index+1 >=  scrollBar.window_left && index+1 <  scrollBar.window_right)
        {
            tab_list.at(index)->setVisible(true);
            tab_list.at(index)->setMaximumWidth(pixel_per_tab);
            tab_list.at(index)->setMinimumWidth(pixel_per_tab);
        }
        else
        {
            tab_list.at(index)->setVisible(false);
        }
        tab_list.at(index)->update();
    }

    mainLayout->update();
    update();


}
void TabList::focus_update()
{
    QWidget * activeTab = QWidget::focusWidget();
    if(activeTab != nullptr)
        activeTab->clearFocus();
    QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");

    if (scrollBar.passiveTab_index>0)
    {
        scrollBar.activeTab_index = scrollBar.passiveTab_index;
        scrollBar.passiveTab_index = 0;
        focus_update();
    }
    else
    {
        if (scrollBar.activeTab_index>0)
        {
            if(scrollBar.activeTab_index > tab_list.length())
            {
                scrollBar.activeTab_index --;
                focus_update();
            }
            else
            {
                tab_list.at(scrollBar.activeTab_index-1)->setFocus();
            }
        }
        else
        {
            mainTab->setFocus();
        }
    }
    update();

    if (mainTab->isVisible())
        emit toParent("arrow_left_passive");
    else
        emit toParent("arrow_left_active");
    if (tab_list.length() >0 &&  tab_list.last()->isVisible())
        emit toParent("arrow_right_passive");
    else
        emit toParent("arrow_right_active");
}


// from parent
void TabList::newTabRequest()
{
    newTab("New App", ":/ui/images/cessor_icons/gray.png");
}
void TabList::updateWidth(int available_width)
{
    setMaximumWidth(available_width);
    update();

    if (scrollBar.isActive)
    {
        available_width -=
                mainLayout->contentsMargins().left() +
                mainLayout->contentsMargins().right();

        if (scrollBar.activeTab_index == 0)   // mainTab
        {
            scrollBar.window_left = 0;
            available_width -= mainTab->width();
            scrollBar.window_right = (int) available_width / MINIMUM_TAB_WIDTH +1;
            visibility();
        }
        else
        {
            int tab_length = (int) available_width / MINIMUM_TAB_WIDTH;
            QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");
            int children = tab_list.length() +1;
            if (tab_length > children)
            {
                listWidth = mainTab->width();
                emit updateDimensions();
            }
            else
            {
                if (scrollBar.activeTab_index - tab_length >= 0)
                {
                    scrollBar.window_left = scrollBar.activeTab_index - tab_length+1;
                    scrollBar.window_right = scrollBar.activeTab_index+1;
                }
                else
                {
                    scrollBar.window_left = 0;
                }
                visibility();
            }
        }
    }
    else
    {
        scrollBar.tabWidth = 0;
        scalability();
    }
    focus_update();

}
void TabList::scroll_left()
{
    if (scrollBar.window_left > 0)
    {
        scrollBar.window_left --;
        scrollBar.window_right --;
        visibility();
    }
    focus_update();

}
void TabList::scroll_right()
{
    QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");

    if (scrollBar.window_right <= tab_list.length())
    {
        scrollBar.window_left ++;
        scrollBar.window_right ++;
        visibility();
    }
    focus_update();
}


//  from children
void TabList::formChildren(QString request, QWidget *child)
{
    if (request == "focus")
    {
        if (child->objectName() == "mainTab")
        {
            scrollBar.activeTab_index = 0;
        }
        else if (child->objectName() == "tabObject")
        {
            QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");
            if (tab_list.indexOf(child) >= 0)
            {
                scrollBar.activeTab_index = tab_list.indexOf(child) +1;
            }
        }
        focus_update();
    }
    else if (request == "unFocus")
    {
        focus_update();
    }
    else if (request == "close")
    {
        QList<QWidget *> tab_list = findChildren<QWidget *>("tabObject");

        if (scrollBar.isActive)
        {
            int available_width =
                    maximumWidth() -
                    mainLayout->contentsMargins().left() -
                    mainLayout->contentsMargins().right();
            int available_tabs = available_width / MINIMUM_TAB_WIDTH;

            if(tab_list.length() > available_tabs)
            {
                scrollBar.passiveTab_index = scrollBar.activeTab_index -1;
                if (scrollBar.window_left >0)
                {
                    scrollBar.window_left --;
                    scrollBar.window_right --;
                }
                child->~QWidget();
                visibility();
                focus_update();

            }
            else
            {
                listWidth = 100;//  exit active mode
                child->~QWidget();
                emit updateDimensions();
            }
        }
        else
        {
            listWidth -= child->width() + mainLayout->spacing();
            child->~QWidget();
            emit updateDimensions();
        }
        mainLayout->update();
        update();
    }
}
