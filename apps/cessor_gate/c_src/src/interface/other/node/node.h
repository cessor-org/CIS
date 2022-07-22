#ifndef NODE_H
#define NODE_H

#include <QObject>
#include <QFile>
#include <QProcess>
#include <QTimer>
#include <QDir>

// CPP time library;    mabe it called by QTimer
//#include <time.h>     /* time_t, struct tm, difftime, time, mktime */

typedef struct {
    bool cli;
    bool gui;
    bool help;
} Interface_type_old;
typedef struct {
    int version;
    QString type;
    QString rpc;
    bool error;
    QByteArray errors;
} FT_object;
class Node : public QObject
{
    Q_OBJECT
public:
    explicit Node(QObject *parent = nullptr);

    void launch(int argc, char *argv[]);


private:
    Interface_type_old interface_type_old;
    void start();
    void state();
    void resolve_state(QByteArray buffer);
    int  launch_cntr;
    void launch_cessor();
    void dock();
    void fault_tolerence(FT_object obj);
signals:

};

#endif // NODE_H
