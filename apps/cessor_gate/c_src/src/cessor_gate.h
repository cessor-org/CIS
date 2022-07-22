// to include the header only once
#pragma once 

#include <erl_nif.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// Type aliases 
typedef void   QApplication;
typedef void   MainWindow;


/*
 *  NIF types
 */
typedef struct
{
    QApplication* app;
    MainWindow* win;
    int test;
} STATE;
typedef struct _qitem_t
{
    struct _qitem_t*    next;
    ErlNifPid*          pid;
} qitem_t;

typedef struct
{
    ErlNifMutex*        lock;
    ErlNifCond*         cond;
    qitem_t*            head;
    qitem_t*            tail;
} queue_t;

typedef struct
{
    ErlNifEnv*          env;
    ErlNifThreadOpts*   opts;
    ErlNifTid           qthread;
    ErlNifTid           appthread;
    ErlNifThreadOpts*   appopts;
    queue_t*            queue;
    ERL_NIF_TERM        atom_ok;
    ErlNifPid*          callback;
    QApplication*       app;
    MainWindow*         win;
} state_t;


// Application
QApplication* qt_app_new();
int  qt_app_exec(QApplication* self);

//  Window
MainWindow* create_main_window();
void show_main_window(MainWindow* main_win);


void from_mainWindow(int x);
void from_mainWindow_thread(queue_t* queue, int x);
void register_callback_thread(
        MainWindow* main_win,
        queue_t* queue,
        void (* callback)(queue_t* queue,int x)
     );
void register_callback(
        MainWindow* main_win,
        void (* callback)(int x)
     );
