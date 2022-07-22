#include <QApplication>
#include "interface_remote.h"
#include <unistd.h>
#include <iostream>
#include <cstdlib>
#include <signal.h>
using namespace std;
// Define the function to be called when ctrl-c (SIGINT) is sent to process
void signal_callback_handler(int signum) {
   cout << "Caught signal " << signum << endl;
   // Terminate program
   //https://stackoverflow.com/questions/7581343/how-to-catch-ctrlc-on-windows-and-linux-with-qt
   exit(signum);
}
int main(int argc, char *argv[])
{
    int argc_app;
    char *argv_app[]={};

    QApplication app(argc, argv);
    qDebug() << "Debug" << "main";
    InterfaceRemote * interface_remote = new InterfaceRemote();
    interface_remote->init(argc, argv);
    signal(SIGINT, signal_callback_handler);
    return app.exec();
}
