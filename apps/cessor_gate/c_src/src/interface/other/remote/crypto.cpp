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
    SSL_free(ssl);
    close(sfd);
    SSL_CTX_free(ctx_ssl);
}

//  Keys
int Crypto::generate_key()
{
    unsigned int primes = 3;
    unsigned int bits = 4096;
    OSSL_PARAM params[3];

    pctx = EVP_PKEY_CTX_new_from_name(NULL, "RSA", NULL);

    EVP_PKEY_keygen_init(pctx);
    //std::cout << "key gen:" << test << '\n';
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
    const char * filePath =
            "./services/systemServices/gate/crypto/key.pem";
    FILE * f;
    f = fopen(filePath, "wb");
    int result = PEM_write_PrivateKey(
        f,                  /* file */
        pkey,               /* key object */
        EVP_des_ede3_cbc(), /* encryption info */
        password,           /* passphrase*/
        pass_len,           /* passphrase length */
        NULL,               /* callback */
        NULL                /* data to pass to the callback */
    );
    fclose(f);
    return result;
}
int Crypto::save_cert()
{
    const char * filePath =
            "./services/systemServices/gate/crypto/certificate.pem";
    FILE * f;
    f = fopen(filePath, "wb");
    int result = PEM_write_X509(
        f,   /* file */
        x509 /* certificate */
    );
    fclose(f);
    return result;
}

//  TLS
int Crypto::init_tls()
{
    ctx_ssl = initSSL_CTX();
    if (ctx_ssl == nullptr)
    {
        return ERR_STATUS;
    }
    ssl = SSL_new(ctx_ssl);
    if (ssl == nullptr)
    {
        return ERR_STATUS;
    }
    //  Connection to interface server with localhost address and 31415 port number
    sfd = OpenConnection("127.0.0.1", "31415");
    SSL_set_fd(ssl, sfd);

    const int status = SSL_connect(ssl);
    if (status != 1)
    {
        SSL_get_error(ssl, status);
        return ERR_STATUS;
    }

    printf("Connected with %s encryption\n", SSL_get_cipher(ssl));
    const char *chars = "Hello World, 123!";
    SSL_write(ssl, chars, strlen(chars));
    return OK_STATUS;
}
SSL_CTX * Crypto::initSSL_CTX(void)
{
    const SSL_METHOD *method = TLS_client_method(); /* Create new client-method instance */
    SSL_CTX *ctx = SSL_CTX_new(method);
    if (ctx == nullptr)
    {
        return nullptr;
    }
    int cert = SSL_CTX_use_certificate(ctx, x509);
    if (cert != 1)//1:success
    {
        return nullptr;
    }
    int private_key = SSL_CTX_use_PrivateKey(ctx, pkey);
    if (private_key != 1)//1:success
    {
        return nullptr;
    }
    return ctx;
}
int Crypto::OpenConnection(const char *hostname, const char *port)
{
    struct hostent *host;
    if (gethostbyname(hostname) == nullptr)
    {
        return ERR_STATUS;
    }

    struct addrinfo hints = {0}, *addrs;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    const int status = getaddrinfo(hostname, port, &hints, &addrs);
    if (status != 0)
    {
        return ERR_STATUS;
    }
    struct addrinfo *addr = addrs;
    int sfd = socket(addrs->ai_family, addrs->ai_socktype, addrs->ai_protocol);
    if (sfd < 0)
    {
        return ERR_STATUS;
    }
    if (connect(sfd, addr->ai_addr, addr->ai_addrlen) != 0)
    {
        close(sfd);
        return ERR_STATUS;
    }
    freeaddrinfo(addrs);
    return sfd;
}
int Crypto::tls_send(QByteArray * CSTX)
{
    int size = CSTX->size();
    const char * chars = CSTX->data();
    return SSL_write(ssl, chars, size);
}
