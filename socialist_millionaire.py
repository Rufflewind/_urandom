#!/usr/bin/env python3
#
# Implements the socialist millionaire protocol:
# https://en.wikipedia.org/wiki/Socialist_millionaires
#
# One participant should run this program as Alice (no -l flag);
# the other participant should run this program as Bob (with -l flag).
#
# Every number Alice prints to stdout should be sent to Bob via stdin, and
# vice versa.
#
import argparse, hashlib, sys
import Crypto.Util.number

def random_nontrivial_exponent(h, p):
    while True:
        a = Crypto.Util.number.getRandomRange(1, p)
        ha = pow(h, a, p)
        if ha != 1:
            return a, ha

def exchange(send, recv, is_bob, x, h, p):
    a, ha = random_nontrivial_exponent(h, p)
    A, hA = random_nontrivial_exponent(h, p)
    send(ha)
    send(hA)
    hb = recv()
    hB = recv()
    if hb == 1 or hB == 1:
        raise Exception("other side did not follow protocol "
                        "and gave us bad exponent(s)")
    g = pow(hb, a, p)
    G = pow(hB, A, p)
    r = Crypto.Util.number.getRandomRange(1, p)
    Pa = pow(G, r, p)
    Qa = pow(h, r, p) * pow(G, x, p) % p
    send(Pa)
    send(Qa)
    Pb = recv()
    Qb = recv()
    if Pa == Pb or Qa == Qb:
        raise Exception("oops, bad P and/or Q; please try again")
    if is_bob:
        Pa, Pb = Pb, Pa
        Qa, Qb = Qb, Qa
    invQb = pow(Qb, p - 2, p)
    invPb = pow(Pb, p - 2, p)
    qq = Qa * invQb % p
    send(pow(qq, A, p))
    qqb = recv()
    return pow(qqb, A, p) == Pa * invPb % p

def send(x):
    sys.stdout.write("{}\n".format(x))
    sys.stdout.flush()

def recv():
    return int(sys.stdin.readline().strip())

def socialist_millionaire(message, is_bob):
    h = 2
    p = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF # RFC 3526 1536-bit modulus
    if not Crypto.Util.number.isPrime(p):
        raise Exception("not a prime: {}", p)
    digest = hashlib.sha512(message).digest()
    x = Crypto.Util.number.bytes_to_long(digest)
    sys.stderr.write(str(exchange(send, recv, is_bob, x, h, p)) + "\n")

p = argparse.ArgumentParser()
p.add_argument("-l", "--is-bob", action="store_true")
p.add_argument("message_file")
args = p.parse_args()
with open(args.message_file, "rb") as f:
    message = f.read()
socialist_millionaire(message, args.is_bob)
