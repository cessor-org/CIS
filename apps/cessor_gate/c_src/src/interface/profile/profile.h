#ifndef PROFILE_H
#define PROFILE_H

#include <QObject>
#include "../interface_state.h"
#include "../io/io.h"
//  Profile callback
typedef struct Profile_cb
{
    int status;     //0:ok; 1:error;
    STATE_t state;
    QList<QString> error;
}
Profile_cb_t;



class Profile : public QObject
{
    Q_OBJECT
public:
    explicit Profile(QObject *parent = nullptr);
    Profile_cb_t task_man(CSTP_cb_task_t task, STATE_t * state);
private:
    QString doc_path = "../../../services/system/gate/print/profile/";
private:
    Profile_cb_t sign(QList<QString> args, STATE_t * state);
    Profile_cb_t signup(STATE_t * state);
    Profile_cb_t signup_pass(STATE_t * state);
    int password_validation(QString pass);
    int passwords_compare(QString pass, QString confirm);
    Profile_cb_t ft(FT_t FT);
signals:

};

//const int LOAD_TYPE_signup = 0;
//const int LOAD_TYPE_login = 1;

#endif // PROFILE_H
