#ifndef CRYPTO_H
#define CRYPTO_H


#include <stdio.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <openssl/rand.h>

class Crypto
{
public:
    explicit Crypto();
    ~Crypto();
    int generate_key();
    int create_cert();
    int save_key(const unsigned char* password, int pass_len);
    int save_cert();

private:
    EVP_PKEY *pkey;
    EVP_PKEY_CTX *pctx;
    X509 * x509;
};

#endif // CRYPTO_H
