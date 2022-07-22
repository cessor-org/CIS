#include "interface_remote.h"

/**********/
/*  API  */
/********/

InterfaceRemote::InterfaceRemote(QObject *parent) : QObject(parent)
{
    cstp = new CSTP(this);
    connect(cstp, SIGNAL(callBack(CSTP_cb_t)), this, SLOT(cstp_callBack(CSTP_cb_t)));
    crypto = new Crypto;
}
InterfaceRemote::~InterfaceRemote()
{
}
bool iequals(std::string& a, std::string& b)
{
    return std::equal(a.begin(), a.end(),
                      b.begin(), b.end(),
                      [](char a, char b) {
                          return tolower(a) == tolower(b);
                      });
}
void InterfaceRemote::init(int argc, char *argv[])
{
    qDebug() << "Debug" << "init";
    bool cli=false,gui=false,help=false;
    if (argc == 2)
    {
        std::string command(argv[1]);
        //  Available commands
        std::string cli("cli");
        std::string gui("gui");

        //  CLI
        if(iequals(command, cli))
        {
            state.remoteType.cli=true;
            // print interface information
            //state_file();
            //Cnode * cNode = new Cnode;
            //cNode->connect0();
        }
        //  GUI
        else if(iequals(command, gui))
        {
            state.remoteType.gui=true;
        }
        //  Unknown
        else
        {
            //  help
            state.remoteType.help=true;
        }
    }
    //  Unknown
    else
    {
        //  help
        state.remoteType.help=true;
    }
}
/**********/
/*  BIF  */
/********/

/*******************************/
/*  InterfaceRemote Synapses  */
/*****************************/
void InterfaceRemote::key_gen()
{
    qDebug() << "Debug" << "key_gen";
    int generate_key = crypto->generate_key();
    if(generate_key > 0)
    {
        int create_cert = crypto->create_cert();
        if(create_cert > 0)
        {
            int test = crypto->save_cert();
            qDebug() << "Debug" << "key_gen" << "save_cert"<<test;
        }
        else
            qDebug() << "Debug" << "key_gen" << "create_cert"<<create_cert;
    }
    else
        qDebug() << "Debug" << "key_gen" << "generate_key"<<generate_key ;
}
void InterfaceRemote::launch_cessor()
{
    qDebug() << "Debug" << "launch_cessor";
    QString cessor_path = "/home/ssd/cessor/_build/prod/rel/cessor/bin/cessor";
    QStringList arguments;
    arguments << "daemon";

    QProcess * launcher = new QProcess(this);
    launcher->start(cessor_path, arguments);

    qDebug() << "Debug" << "launch_cessor"
             << launcher->error()
             <<launcher->isReadable()
             <<launcher->isWritable()
            <<launcher->write("abcd")
           <<launcher->children()
          <<launcher->atEnd()
            <<launcher->processChannelMode()
              <<launcher->isOpen()
             <<launcher->isTransactionStarted()
            <<launcher->dynamicPropertyNames();
    QObject::connect(
                launcher
                , qOverload<int, QProcess::ExitStatus >(&QProcess::finished)
                , this
                , &InterfaceRemote::on_launcher_finished);

}
void InterfaceRemote::on_launcher_finished()
{
    qDebug() << "Debug" << "on_launcher_finished";
}
void InterfaceRemote::state_file()
{
    qDebug() << "Debug" << "state_file";
    //  The state file of interface has to be found on
    //  "./opt/cessor/state/interface"
    QDir * statePath = new QDir();
    statePath->cd("/");
    statePath->cd("opt");
    statePath->cd("cessor");
    statePath->cd("state");
    QString interfacePort = statePath->path();
    interfacePort.append("interface");

    interfacePort.clear();
    interfacePort.append("./services/systemServices/gate/state/INTERFACE");

    //statePath->~QDir();

    QFile file(interfacePort);
    if (file.open(QIODevice::ReadOnly))
    {
        //interfacePort.~QString();
        QByteArray CSTX = file.readAll();

        file.close();
        //file.~QFile();

        QString caller("state");
        qDebug() << "Debug" << "state_file" << "0";
        if (cstp->permission(&CSTX, caller))
        {
            qDebug() << "Debug" << "state_file" << "1";
            cstp->compute(&CSTX, &state);
        }
        else
        {
            qDebug() << "Debug" << "bad state_file";
        }
    }
    else
    {
        qDebug() << "Debug" << "state_file open file problem";
    }
}
//  Call back from connected CSTP objects
void InterfaceRemote::cstp_callBack(CSTP_cb_t cb)
{
    qDebug() << "Debug" << "cstp";
    //  Status OK
    if (cb.status == 0)
    {
        for (int i = 0; i < cb.tasks.size(); ++i) {
            CSTP_cb_task_t task = cb.tasks.at(i);
            task_man(task);
        }
    }
    //  Status ERROR
    else if (cb.status == 1)
    {
    }
    //  Unknown
    else
    {
        //  fault tolerance
    }
}
//  CIS Task Manager
void InterfaceRemote::task_man(CSTP_cb_task_t task)
{
    qDebug() << "Debug" << "task_man" << task.name;
    //  Generate new keys
    if (task.name == "key_gen")
    {
        if (task.delay == 0)
            key_gen();
        else
            QTimer::singleShot(task.delay, this, &InterfaceRemote::key_gen);
    }
    //  Launch cessor
    if (task.name == "launch_cessor")
    {
        if (task.delay == 0)
            launch_cessor();
        else
            QTimer::singleShot(task.delay, this, &InterfaceRemote::launch_cessor);
    }
    //  Read state_file
    else if (task.name == "state_file0")
    {
        if (task.delay == 0)
            state_file();
        else
            QTimer::singleShot(task.delay, this, &InterfaceRemote::state_file);
    }
    //  Unknown
    else
    {
        //  fault tolerance
    }
}
