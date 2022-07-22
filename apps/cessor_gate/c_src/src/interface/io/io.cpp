#include "io.h"
#include <iostream>

//#include <iostream>
//#include <string>
//#include "hidden_service_input.hpp"

/*************/
/*  PUBLIC  */
/***********/

IO::IO(QObject *parent)
    : QObject{parent}
{
}

QString IO::input(int type)
{
    if (type == INPUT_hidden)
    {
        INPUT_service * in = new INPUT_service();
        std::string str = in->input_sec();
        //in->~INPUT_service();
        QString qstr = QString::fromStdString(str);
        return qstr;
    } else  //(type == INPUT_normal)
    {
        return "ok";
    }
}
int IO::print(int type)
{
    if (type == PRINT_splash)
    {
        QString fName("../../../services/system/gate/print/SPLASH");
        QFile file(fName);
        if (!file.open(QIODevice::ReadOnly))
            return ERR_STATUS;
        QByteArray FILE = file.readAll();
        file.close();
        std::cout << FILE.toStdString() <<"\n";
        return OK_STATUS;
    }
    else if(type == PRINT_help)
    {
        QString fName("../../../services/system/gate/print/HELP");
        QFile file(fName);
        if (!file.open(QIODevice::ReadOnly))
            return ERR_STATUS;
        QByteArray FILE = file.readAll();
        file.close();
        std::cout << FILE.toStdString() <<"\n";
        return OK_STATUS;
    }
    else
        return ERR_STATUS;
}

int IO::print_file(QString path)
{
    QFile file(path);
    if (!file.open(QIODevice::ReadOnly))
        return ERR_STATUS;
    QByteArray FILE = file.readAll();
    file.close();
    std::cout << FILE.toStdString();
    return OK_STATUS;
}
/**************/
/*  PRIVATE  */
/************/
