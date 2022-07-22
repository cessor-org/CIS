/*
 *
 *  Cessor Information Systems
 *  Cessor Service Transaction Protocol
 *
 *  Version Crown-Block index = 0
 *  Version Serial Domain = 0
 *  Version Serial number = 0
 *
 *  Service Domain = 0
 *  Service Sub-Domain = 0; System service
 *
 *  Procedure Range = 3;  Gate Packages Range
 *  Procedure Package = 3;  Interface Remote package
 *
 *  Process Module = 0;     State module
 *
 */
#include <time.h>

#include "cstp_000000_0000_0303_00.h"

CSTP_000000_0000_0303_00::CSTP_000000_0000_0303_00(QObject *parent) : QObject(parent)
{

}
CSTP_000000_0000_0303_00::~CSTP_000000_0000_0303_00()
{

}
void CSTP_000000_0000_0303_00::compose()
{

}
CSTP_cb CSTP_000000_0000_0303_00::compute(QByteArray * CSTX, STATE_t * state)
{
    if (CSTX->length()>0)
    {
        // Processor Reference
        int pRef = CSTX->at(0);
        //  Interface Status Processor Reference
        if (pRef == 0)
        {
            CSTX->remove(0,1);
            return processor_ref0(CSTX, state);
        }
        else
        {
            CSTP_cb cb;
            cb.status = 1;
            //cb.error.clear();
            return cb;
        }
    }
    else
    {
        CSTP_cb cb;
        cb.status = 1;
        //cb.error.clear();
        return cb;
    }

}

/**********/
/*  BIF  */
/********/

/**********************/
/*  State Processors */
/********************/

//  Interface Status Processor Reference
bool processor_ref0_isValid(QByteArray * CSTX)
{
    if (CSTX->length()<7)
    {
        return false;
    }

    time_t now;
    time(&now);  /* get current time; same as: now = time(NULL)  */

    struct tm timeStamp;
    //tm * timeStamp = new tm;
    timeStamp = *localtime(&now);

    //  Year
    int year1 = (int)(unsigned char)CSTX->at(0);
    int year2 = (int)(unsigned char)CSTX->at(1);
    int year = year1 * 256 + year2;
    timeStamp.tm_year = year-1900;

    //  Month
    int mon = (int)(unsigned char)CSTX->at(2);//1-12
    timeStamp.tm_mon = mon-1;//0-11

    //  Day
    int day = (int)(unsigned char)CSTX->at(3);
    timeStamp.tm_mday = day;

    //  Hour
    int hour = (int)(unsigned char)CSTX->at(4);
    timeStamp.tm_hour = hour;

    //  Minute
    int min = (int)(unsigned char)CSTX->at(5);
    timeStamp.tm_min = min;

    //  Second
    int sec = (int)(unsigned char)CSTX->at(6);
    timeStamp.tm_sec = sec;

    double seconds = difftime(now, mktime(&timeStamp));

    if (seconds<0)
    {
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "invalid <0"<<seconds;
        return false;
    }
    if (seconds>180)    // Base Block period
    {
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "invalid >180"<<seconds;
        return false;
    }
    CSTX->remove(0,7);
    qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "valid"<<seconds;
    //timeStamp.~tm();
    return true;
}
CSTP_cb CSTP_000000_0000_0303_00::processor_ref0(QByteArray * CSTX, STATE_t * state)
{
    CSTP_cb cb;
    if (CSTX->length()==0)
    {
        cb.status = 1;
        //cb.error.clear();
        return cb;
    }
    //  Status of interface
    // 0:genesis; 1:init; 2:launch_cessor;
    int status = CSTX->at(0);
    CSTX->remove(0,1);

    //  Genesis status
    if(status == 0)
    {
        if (CSTX->length()>0)
        {
            cb.status = 1;
            //cb.error.clear();
            return cb;
        }
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "status genesis";
        //  Status
        cb.status=0;    //  OK
        //  generate new keys for tls connection
        CSTP_cb_task_t key_gen;
        key_gen.name = "key_gen";
        key_gen.delay = 0;
        cb.tasks.insert(0, key_gen);
        //  launch_cessor cessor
        CSTP_cb_task_t launch_cessor;
        launch_cessor.name = "launch_cessor";
        launch_cessor.delay = 0;
        cb.tasks.insert(1, launch_cessor);
        //  Read the status later
        CSTP_cb_task_t state_file;
        state_file.name = "state_file";
        //  Set a delay of 5 sec
        state_file.delay = 5000;
        cb.tasks.insert(2, state_file);
    }
    //  Init status
    else if(status == 1)
    {
        bool validate = processor_ref0_isValid(CSTX);
        if(!validate)
        {
            cb.status = 1;
            return cb;
        }
        qDebug() << "Debug" << "CSTP_000000_0000_0303_00" << "status init";
        //  Status
        cb.status=0;    //  OK
        //  launch_cessor cessor
        //      In case of crash; it helps to relaunch_cessor
        CSTP_cb_task_t re_launch_cessor;
        re_launch_cessor.name = "launch_cessor";
        re_launch_cessor.delay = 0;
        cb.tasks.insert(0, re_launch_cessor);
        //  Read the status later
        CSTP_cb_task_t state_file;
        state_file.name = "state_file";
        //  Set a delay of 3 sec
        state_file.delay = 3000;
        cb.tasks.insert(1, state_file);
    }
    //  launch_cessor status
    else if(status == 2)
    {
        bool validate = processor_ref0_isValid(CSTX);
        if(!validate)
        {
            cb.status = 1;
            return cb;
        }
        //  Status
        cb.status=0;    //  OK
    }
    else
    {
        cb.status = 1;
        //cb.error.clear();
        return cb;
    }
    return cb;
}

