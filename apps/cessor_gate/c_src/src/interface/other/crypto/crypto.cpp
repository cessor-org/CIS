#include "crypto.h"
#include <iostream>
Crypto::Crypto()
{
    pkey = NULL;
    std::cout << "crypto inited!" << '\n';
}
Crypto::~Crypto()
{
    EVP_PKEY_CTX_free(pctx);
}
int Crypto::generate_key()
{
    unsigned int primes = 3;
    unsigned int bits = 4096;
    OSSL_PARAM params[3];

    pctx = EVP_PKEY_CTX_new_from_name(NULL, "RSA", NULL);

    EVP_PKEY_keygen_init(pctx);

    params[0] = OSSL_PARAM_construct_uint("bits", &bits);
    params[1] = OSSL_PARAM_construct_uint("primes", &primes);
    params[2] = OSSL_PARAM_construct_end();
    EVP_PKEY_CTX_set_params(pctx, params);

    return EVP_PKEY_generate(pctx, &pkey);
}
int Crypto::create_cert()
{
    x509 = X509_new();
    ASN1_INTEGER_set(X509_get_serialNumber(x509), 1);
    X509_gmtime_adj(X509_get_notBefore(x509), 0);
    X509_gmtime_adj(X509_get_notAfter(x509), 31536000L);
    X509_set_pubkey(x509, pkey);
    X509_NAME * name;
    name = X509_get_subject_name(x509);
    X509_NAME_add_entry_by_txt(name, "C",  MBSTRING_ASC,
                               (unsigned char *)"InterSpace", -1, -1, 0);
    X509_NAME_add_entry_by_txt(name, "O",  MBSTRING_ASC,
                               (unsigned char *)"Cessor Org.", -1, -1, 0);
    X509_NAME_add_entry_by_txt(name, "CN", MBSTRING_ASC,
                               (unsigned char *)"cessor", -1, -1, 0);
    X509_set_issuer_name(x509, name);
    return X509_sign(x509, pkey, EVP_sha1());
}
int Crypto::save_key(const unsigned char* password, int pass_len)
{
    FILE * f;
    f = fopen("./certs/key.pem", "wb");
    return PEM_write_PrivateKey(
        f,                  /* file */
        pkey,               /* key object */
        EVP_des_ede3_cbc(), /* encryption info */
        password,           /* passphrase*/
        pass_len,           /* passphrase length */
        NULL,               /* callback */
        NULL                /* data to pass to the callback */
    );
}
int Crypto::save_cert()
{
    FILE * f;
    f = fopen("./certs/certificate.pem", "wb");
    return PEM_write_X509(
        f,   /* file */
        x509 /* certificate */
    );
}
