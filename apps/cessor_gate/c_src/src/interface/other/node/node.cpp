#include "node.h"
#include <QDebug>

Node::Node(QObject *parent) : QObject(parent)
{

}
void Node::launch(int argc, char *argv[])
{
    bool cli=false,gui=false,help=false;
    if (argc == 2)
    {
        std::string command(argv[1]);

        if (command == "cli" || command == "CLI")
        {
            //  CLI
            interface_type_old.cli=true;
        }
        else if (command == "gui" || command == "GUI")
        {
            //  GUI
            interface_type_old.gui=true;
        }
        else
        {
            //  help
            interface_type_old.help=true;
        }
    }
    else
    {
        //  help
        interface_type_old.help=true;
    }
    start();
}
void Node::start()
{
    if (interface_type_old.cli || interface_type_old.gui)
    {
        state();
    }
    else
    {
        // print help menu
    }
}
void Node::state()
{
    //  The state file of interface has to be found on
    //  "./opt/cessor/state/interface"
    QDir * statePath = new QDir();
    statePath->cd("/");
    statePath->cd("opt");
    statePath->cd("cessor");
    statePath->cd("state");
    QString interfacePort = statePath->path();
    interfacePort.append("interface");

    statePath->~QDir();

    QFile file(interfacePort);
    if (file.open(QIODevice::ReadOnly))
    {
        interfacePort.~QString();
        QByteArray buffer = file.readAll();
        file.close();
        file.~QFile();
        resolve_state(buffer);
    }
    else
    {
        FT_object ft;
        ft.type = "state_file";
        fault_tolerence(ft);
    }
}
void Node::resolve_state(QByteArray buffer)
{
    int version = buffer.at(0);
    if(version == 0)//  version 0
    {
        unsigned char type = buffer.at(1);
        if((int)type == 255)//   system services
        {
            unsigned char micro_service = buffer.at(2);
            if((int)micro_service == 0)//   interface services
            {
                unsigned char rpc = buffer.at(3);
                if((int)rpc == 0)//   log service
                {
                    unsigned char flag_len = buffer.at(4);
                    if((int)flag_len == 1)//   length of flags
                    {
                        unsigned char flag = buffer.at(5);
                        if((int)flag == 0)//    init
                        {
                            launch_cessor();
                        }
                        else if((int)flag == 1)//    run
                        {
                            launch_cessor();
                        }
                        else
                        {
                            //  unsupported flag
                            qDebug() << "unsupported flag";
                        }
                    }
                    else
                    {
                        //  unsupported flag_len
                        qDebug() << "unsupported flag_len";
                    }
                }
                else
                {
                    //  unsupported rpc
                    qDebug() << "unsupported rpc";
                }
            }
            else
            {
                //  unsupported micro_service
                qDebug() << "unsupported micro_service";
            }
        }
        else
        {
            //  unsupported type
            qDebug() << "unsupported type: " <<type<<buffer.length();
        }
    }
    else
    {
        //  unsupported version
        qDebug() << "unsupported version";
    }
}
void Node::launch_cessor()
{
    qDebug() << "debug 0";
    QString program = "/home/ssd/cessor/_build/prod/rel/cessor/bin/cessor";
    QStringList arguments;
    arguments << "daemon";

    QProcess *myProcess = new QProcess(this);
    myProcess->start(program, arguments);
    QTimer::singleShot(1000, this, &Node::dock);
    qDebug() << "debug 1";
}
void Node::dock()
{
    qDebug() << "debug 2";
}
void Node::fault_tolerence(FT_object obj)
{
    qDebug() << "FT got: " << obj.type;
}
void timer() {
    /* difftime example */
      time_t now;

      double seconds;

      time(&now);  /* get current time; same as: now = time(NULL)  */

      struct tm genesisTime;
      genesisTime = *localtime(&now);

      genesisTime.tm_sec = 0;
      genesisTime.tm_min = 0;
      genesisTime.tm_hour = 0;
      genesisTime.tm_mday = 0;
      genesisTime.tm_mon = 0;
      genesisTime.tm_year = 2022;


      seconds = difftime(now,mktime(&genesisTime));

      printf ("%.f seconds since new year in the current timezone.\n", seconds);
}
