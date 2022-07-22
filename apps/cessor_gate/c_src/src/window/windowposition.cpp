#include "windowposition.h"

WindowPosition::WindowPosition(QWidget *parent) : QWidget(parent)
{
    QList<QScreen *> screens        = QGuiApplication::screens();
    QScreen *        first_screen   = screens.first();

    screen = first_screen->availableGeometry();
    qDebug() << "screen" << "availableGeometry" << screen.y();

    MinimumWidth = 720;
    MinimumHeight = 680;

    win_pos.setTop(200);
    win_pos.setLeft(500);
    win_pos.setWidth(MinimumWidth);
    win_pos.setHeight(MinimumHeight);

    margins.setTop(0);
    margins.setRight(5);
    margins.setBottom(5);
    margins.setLeft(5);

    onResize.isActive   =false;
    onResize.top        =false;
    onResize.right      =false;
    onResize.bottom     =false;
    onResize.left       =false;

    onMove              =false;

    isMaximized         =false;
}
WindowPosition::~WindowPosition()
{

}
void WindowPosition::init()
{
    emit upgradeGeometry(win_pos);
    emit upgradeMargins(margins);

    QMargins test(5,0,5,5);

    /*
    qDebug() << "WindowPosition" <<  "init"
             << "is differnt:" << operator!=(margins, test);
    */
}
//  Mouse events
void WindowPosition::mousePressEvent(QMouseEvent *e)
{

    int win_w = win_pos.width();
    int win_h = win_pos.height();

    QPoint position = e->pos();

    if (position.y() <= margins.top())
    {
        onResize.isActive = true;
        onResize.top =      true;
    }
    else if (position.x() >= win_w - margins.right())
    {
        onResize.isActive = true;
        onResize.right =    true;
    }
    else if (position.y() >= win_h - margins.bottom())
    {
        onResize.isActive = true;
        onResize.bottom =   true;
    }
    else if (position.x() <= margins.left())
    {
        onResize.isActive = true;
        onResize.left =     true;
    }
    else
        onMove = true;

    qDebug() << "WindowPosition" << "mousePressEvent"
             << "position" << position
             << "isActive" << onResize.isActive
             << "top" << onResize.top
                << "right" << onResize.right
                   << "left" << onResize.left
                      << "bottom" << onResize.bottom;


    oldPosition = position;
}
void WindowPosition::mouseReleaseEvent(QMouseEvent *e)
{
    //QRect test(win_x, win_y, win_w, win_h);
    qDebug() << "WindowPosition" <<  "mouseReleaseEvent" << win_pos;
    oldPosition.setX(0);
    oldPosition.setY(0);
    if (onResize.isActive)
    {
        onResize.isActive = false;
        onResize.top =      false;
        onResize.right =    false;
        onResize.bottom =   false;
        onResize.left =     false;
    } else
        onMove = false;
}
void WindowPosition::mouseDoubleClickEvent(QMouseEvent *e)
{
    qDebug() << "WindowPosition" <<  "mouseDoubleClickEvent";
}
void WindowPosition::mouseMoveEvent(QMouseEvent *e)
{
   //qDebug() << "WindowPosition " << "mouseMoveEvent" << onResize.isActive << onMove;
    int win_x = win_pos.x();
    int win_y = win_pos.y();
    int win_w = win_pos.width();
    int win_h = win_pos.height();


    QPoint position = e->pos();


    if (onResize.isActive)
    {
        //  Y Axis
        if (onResize.top || onResize.bottom)
        {
            int yChange = position.y() - oldPosition.y();

            if (yChange < 0)
            {
                //  Go to top
                if (onResize.top)
                {
                    if (win_y + yChange >= screen.y())
                    {
                        win_y += yChange;
                        win_h -= yChange;
                    }
                    else
                    {
                        win_h += win_y - screen.y();
                        win_y = screen.y();
                    }
                }
                else if (onResize.bottom)
                {
                    if (win_h > MinimumHeight)
                    {
                        win_h += yChange;

                    }
                    oldPosition.setX(position.x());
                    oldPosition.setY(position.y());
                }
            }
            else if (yChange > 0)
            {
                //  Go to down

                if (onResize.top)
                {
                    if( win_h > MinimumHeight )
                    {
                        if (win_h - yChange >= MinimumHeight)
                        {
                            win_y += yChange;
                            win_h -= yChange;
                        }
                        else
                        {
                            win_y += win_h - MinimumHeight;
                            win_h = MinimumHeight;
                        }

                    }
                }
                else if (onResize.bottom)
                {
                    int available_space = screen.height() + screen.y() - win_y - win_h;
                    if (yChange <= available_space)
                    {
                        win_h += yChange;
                    }
                    else
                    {
                        win_h = screen.height() + screen.y() - win_y;
                        //win_y = screen.height() + screen.y();
                    }
                    //oldPosition.setX(position.x());
                    oldPosition.setY(position.y());
                }

            }
            cursor.setShape(Qt::SizeVerCursor);
        }
        //  X Axis
        else if (onResize.left || onResize.right) {
            int xChange = position.x() - oldPosition.x();
            //qDebug() << "xChange " << xChange;
            if (xChange < 0)
            {
                //  Go to left
                if (onResize.left)
                {
                    if (win_x + xChange >= screen.x())
                    {
                        win_x += xChange;
                        win_w -= xChange;
                    }
                    else
                    {
                        win_w += win_x - screen.x();
                        win_x = screen.x();
                    }
                }
                else if (onResize.right)
                {
                    if (win_w > MinimumWidth)
                    {
                        win_w += xChange;
                    }
                    oldPosition.setX(position.x());
                    oldPosition.setY(position.y());
                }
            }
            else if (xChange > 0)
            {
                //  Go to right

                if (onResize.left)
                {
                    if(win_w > MinimumWidth)
                    {
                        win_x += xChange;
                        win_w -= xChange;
                    }
                }
                else
                {
                    int available_space = screen.width() - win_x - win_w;
                    if (xChange <= available_space)
                    {
                        win_w += xChange;
                    }
                    else
                    {
                        win_w = screen.width() - win_x;
                    }
                    oldPosition.setX(position.x());
                }
            }
            cursor.setShape(Qt::SizeHorCursor);
        }

        win_pos.setLeft(win_x);
        win_pos.setTop(win_y);
        win_pos.setWidth(win_w);
        win_pos.setHeight(win_h);

        emit upgradeGeometry(win_pos);
        emit updateCursor(cursor);

        check_margins();

    }
    else if (onMove)
    {
        //  X Axis
        int xChange = position.x() - oldPosition.x();
        //qDebug() << "xChange " << xChange;
        if (xChange == 0)
        {
            //  Do nothing
        }
        else if (xChange < 0)
        {
            //  Go to left
            //qDebug() << "left xChange " << win_x + xChange << win_w;

            if (win_x + xChange >= 0)
                win_x += xChange;
                //win_pos.setX(test);
                //win_pos.moveLeft(newX);
            else
                win_x = 0;
        }
        else
        {
            //  Go to right
            int available_space = screen.width() - win_x - win_w;
            if (xChange >= available_space)
            {
                win_x = screen.width() - win_w;
                //win_pos.moveRight(newX);
                //win_pos.setX(screen_width - win_pos.width());
            }
            else
            {
                win_x += xChange;
                //win_pos.setX(win_pos.x() + xChange);
                //win_pos.moveRight(newX);
            }

        }
        //  Y Axis
        int yChange = position.y() - oldPosition.y();
        //qDebug() << "yChange " << yChange;
        if (yChange == 0)
        {
            //  Do nothing
        }
        else if (yChange < 0)
        {
            //  Go to top
            //qDebug() << "top yChange " << y + yChange;
            if (win_y + yChange >= 0)
                win_y += yChange;
            else
                win_y = 0;
        }
        else
        {
            //  Go to down

            int available_space = screen.height() + screen.y() - win_y - win_h;
            //qDebug() << "down yChange " << available_space << win_y << yChange << win_h << screen.height();
            if (yChange >= available_space)
                win_y = screen.height() + screen.y() - win_h;
            else
                win_y += yChange;
        }

        win_pos.setLeft(win_x);
        win_pos.setTop(win_y);
        win_pos.setWidth(win_w);
        win_pos.setHeight(win_h);

        emit upgradeGeometry(win_pos);
        check_margins();

    }
    else
    {
    /*
        qDebug() << "WindowPosition " << "mouseMoveEvent" <<
                    "position:" << position.y() <<
                    "win_w:" << win_h <<
                    "margins.right:" <<  margins.right() <<
                    "margins.left:" << margins.left();
    */

        if (e->windowPos().y() <= margins.top())
            cursor.setShape(Qt::SizeVerCursor);
        else if (position.x() >= win_w - margins.right())
            cursor.setShape(Qt::SizeHorCursor);
        else if (position.y() >= win_h - margins.bottom())
            cursor.setShape(Qt::SizeVerCursor);
        else if (position.x() <= margins.left())
            cursor.setShape(Qt::SizeHorCursor);
        else
            cursor.setShape(Qt::ArrowCursor);

        emit updateCursor(cursor);
    }
}
void WindowPosition::check_margins()
{
    if (screen.y() >= win_pos.y())
        margins.setTop(0);
    else
        margins.setTop(5);
    if (screen.width() <= win_pos.x() + win_pos.width() )
        margins.setRight(0);
    else
        margins.setRight(5);
    if ( screen.height() + screen.y() <= win_pos.y() + win_pos.height() )
        margins.setBottom(0);
    else
        margins.setBottom(5);
    if (win_pos.x() == 0)
        margins.setLeft(0);
    else
        margins.setLeft(5);
    emit upgradeMargins(margins);
}
void WindowPosition::resizeEvent(QRect rect)
{
    //qDebug() << "WindowPosition" << "resizeEvent" << rect ;
    //win_pos = rect;
    //check_margins();
}

void WindowPosition::moveEvent(QRect rect)
{
    //qDebug() << "WindowPosition" << "moveEvent" << rect ;
    win_pos = rect;
    check_margins();
}

//  Control buttons
void WindowPosition::minimize()
{
    qDebug() << "WindowPosition" << "minimized" ;
    emit updateWidowState(Qt::WindowMinimized);
}
void WindowPosition::maximize()
{
    Qt::WindowStates flags = windowState();
    qDebug() << "WindowPosition" << "maximized" <<flags;

    if (isMaximized)
    {
        isMaximized = false;
        emit updateWidowState(Qt::WindowNoState);
    }
    else
    {
        isMaximized = true;
        emit updateWidowState(Qt::WindowMaximized);
    }
}
