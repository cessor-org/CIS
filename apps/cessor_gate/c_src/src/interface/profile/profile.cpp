#include "profile.h"
#include <QDebug>

Profile::Profile(QObject *parent)
    : QObject{parent}
{

}
Profile_cb_t Profile::task_man(CSTP_cb_task_t task, STATE_t * state)
{

    if (task.name == "sign"){
        return sign(task.args, state);
    }
    Profile_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.append("task_name");
    return cb;
}

/**************/
/*  PRIVATE  */
/************/
Profile_cb_t Profile::sign(QList<QString> args, STATE_t * state)
{
    QString sign_type = args[0];
    if (sign_type == "signup")
    {
        return signup(state);
    }
    Profile_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.insert(0, "sign_type");
    return cb;
}
Profile_cb_t Profile::signup(STATE_t * state)
{

    FT_t FT;
    FT.issuer = "signup";

    IO * io = new IO(this);

    QString service_splash(doc_path);
    service_splash.append("SPLASH");
    if (io->print_file(service_splash) == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.insert(0, "splash");
        return ft(FT);
    }

    QString signup(doc_path);
    signup.append("SIGNUP");
    if (io->print_file(signup) == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.insert(0, "SIGNUP_print");
        return ft(FT);
    }
    QString pass_info(doc_path);
    pass_info.append("pass_info");
    if (io->print_file(pass_info) == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.insert(0, "pass_info");
        return ft(FT);
    }
    return signup_pass(state);
}
Profile_cb_t Profile::signup_pass(STATE_t * state)
{

    FT_t FT;
    FT.issuer = "signup";

    IO * io = new IO(this);

    QString pass_new(doc_path);
    pass_new.append("pass_new");
    if (io->print_file(pass_new) == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.insert(0, "pass_new");
        return ft(FT);
    }
    pass_new = io->input(INPUT_hidden);
    if (password_validation(pass_new) == ERR_STATUS)
    {
        QString pass_wrong(doc_path);
        pass_wrong.append("pass_wrong");
        if (io->print_file(pass_wrong) == ERR_STATUS)
        {
            FT.type = "error";
            FT.fault.insert(0, "pass_wrong");
            return ft(FT);
        }
        return signup_pass(state);
    }

    QString confirm(doc_path);
    confirm.append("confirm");
    if (io->print_file(confirm) == ERR_STATUS)
    {
        FT.type = "error";
        FT.fault.insert(0, "pass_repeat");
        return ft(FT);
    }
    confirm = io->input(INPUT_hidden);

    if (passwords_compare(pass_new,confirm) == ERR_STATUS)
    {
        QString confirm_wrong(doc_path);
        confirm_wrong.append("confirm_wrong");
        if (io->print_file(confirm_wrong) == ERR_STATUS)
        {
            FT.type = "error";
            FT.fault.insert(0, "confirm_wrong");
            return ft(FT);
        }
        return signup_pass(state);
    }
    state->crypto.secret = pass_new;
    Profile_cb_t cb;
    cb.status = OK_STATUS;
    // qDebug() << "pass_new" << pass_new << "confirm" << confirm;
    return cb;
}
int Profile::password_validation(QString pass)
{
    //if (pass.length() < 10)
    if (pass.length() < 5)
        return ERR_STATUS;
    int upper=0, lower=0, digit=0, symbol=0;
    for ( const auto& character : pass )
    {
        if ( character.isSpace() )
            return ERR_STATUS;
        if ( character.isUpper() )
            upper++;
        else if ( character.isLower() )
            lower++;
        else if ( character.isDigit() )
            digit++;
        else
            symbol++;
    }
    if (
            upper > 0
            && lower > 0
            && digit > 0
            && symbol > 0
        )
        return OK_STATUS;
    return ERR_STATUS;
}
int Profile::passwords_compare(QString pass, QString confirm)
{
    if (pass.length() != confirm.length() )
        return ERR_STATUS;
    if (QString::compare(pass, confirm, Qt::CaseInsensitive) != OK_STATUS)
        return ERR_STATUS;
    return OK_STATUS;
}
Profile_cb_t Profile::ft(FT_t FT)
{
    Profile_cb_t cb;
    cb.status = ERR_STATUS;
    cb.error.insert(0, FT.type);
    cb.error.insert(1, "profile");
    cb.error.insert(2, FT.issuer);

    for(int index=0; FT.fault.length(); index++)
    {
        QString fault = FT.fault.at(index);
        cb.error.insert(3+index, fault);
    }
    return cb;
}
