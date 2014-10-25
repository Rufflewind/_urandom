#!/bin/sh
set -e

# file name of the certificate excluding the extension
# (the script will produce *.crt and *.key and use an intermediate *.tmp)
out_name=example

# Common Name: the name of the host on which the certificate is valid
cert_common_name=localhost

# strength of the key
cert_key_length=2048

# days before the certificate expires
cert_expire_days=1095

method1() {
    # generate a key in the traditional SSLeay format
    openssl genrsa -out "$out_name.key" "$cert_key_length"
    # generate a certificate signing request
    openssl req -new -key "$out_name.key" -out "$out_name.tmp"      \
                -subj "/CN=$cert_common_name"
    # self-sign the certificate
    openssl x509 -req -days "$cert_expire_days" -in "$out_name.tmp" \
                 -signkey "$out_name.key" -out "$out_name.crt"
    rm "$out_name.tmp"
}

method2() {
    # generate a self-signed certificate with the key stored in PKCS#8 format
    openssl req -days "$cert_expire_days"      \
                -keyout "$out_name.tmp"        \
                -newkey "rsa:$cert_key_length" \
                -nodes                         \
                -out "$out_name.crt"           \
                -subj "/CN=$cert_common_name"  \
                -x509
    # convert the key from PKCS#8 to the traditional SSLeay format
    # because the Haskell library (x509-store) doesn't support PKCS#8
    openssl rsa -in "$out_name.tmp" -out "$out_name.key"
    rm "$out_name.tmp"
}

method2
