#ifndef CRYPTO_H
#define CRYPTO_H

#include <QObject>

#include <stdio.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <openssl/rand.h>


#include <unistd.h>
#include <string.h>

#include <netdb.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#include "../interface_state.h"

class Crypto
{
public:
    explicit Crypto();
    ~Crypto();
    int generate_key();
    int create_cert();
    int save_key(const unsigned char *, int );
    int save_cert();
    int init_tls();
    int tls_send(QByteArray *);

private:
    EVP_PKEY * pkey;
    EVP_PKEY_CTX * pctx;
    X509 * x509;

    SSL_CTX * ctx_ssl;
    SSL * ssl;
    int sfd;

    SSL_CTX * initSSL_CTX(void);
    int OpenConnection(const char *, const char *);

};

#endif // CRYPTO_H
