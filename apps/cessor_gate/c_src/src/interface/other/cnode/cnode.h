#ifndef CNODE_H
#define CNODE_H

typedef struct
{
    int version;
    int type;
    int micro_service;
    int rpc;
    int flag_len;
    unsigned char flags[255];
    bool error;
    unsigned char errors;

} CSTP_OB;

class CNode
{
public:
    CNode();
    ~CNode();
    connect();
};

#endif // CNODE_H
