#ifndef IO_H
#define IO_H

#include <QObject>
#include <QFile>
#include "input_service.h"
#include "../interface_state.h"

using namespace std;

class IO : public QObject
{
    Q_OBJECT
public:
    explicit IO(QObject *parent = nullptr);
    QString input(int type);
    int print(int type);
    int print_file(QString path);
private:

signals:

};

//  Input types
const int INPUT_hidden = 0;
const int INPUT_normal = 1;
//  Print templates
const int PRINT_splash = 0;
const int PRINT_help = 1;



#endif // IO_H
