#!/usr/bin/env python

# This is a script that attempts to analyze the frequency of characters in a
# base32 encoded string that's not an event multiple of 8 using brute-force;
# unfortunately it takes too long to calculate anything past 3 chars, but the
# conclusion is pretty obvious: base32 does have the nice property that in the
# worst case only the last character has a uneven distribution.  In particular
# the unevenness is exactly what you would expect given the number of
# "leftover bits".  This should probably be obvious in retrospect, since 32 is
# a power of 2.
#
# What this means in practice:
#
#   - for MD5 in base32, the last character is not very random as it can only be
#     one of "aeimquy4"
#   - SHA1 is unaffected since its byte-length is a multiple of 5
#   - in both SHA256 and SHA512 the last letter will be poorly distributed
#     (in particular, in SHA256, the last letter can only be 'a' or 'q'!)

import base64, itertools

def gather_freqs()
    length = 3
    elength = 8
    freqs = [{} for _ in range(elength)]
    for s in itertools.product(range(256), repeat=length):
        s = bytearray(s)
        es = base64.b32encode(s).lower()
        for i, c in enumerate(es):
            try:
                freqs[i][c] += 1
            except KeyError:
                freqs[i][c] = 1
    print(freqs)

def analyze_freqs():
    fq1 = [{'a': 8, 'b': 8, 'c': 8, 'd': 8, 'e': 8, 'f': 8, 'g': 8, 'h': 8, 'i': 8, 'j': 8, 'k': 8, 'l': 8, 'm': 8, 'n': 8, 'o': 8, 'p': 8, 'q': 8, 'r': 8, 's': 8, 't': 8, 'u': 8, 'v': 8, 'w': 8, 'x': 8, 'y': 8, 'z': 8, '2': 8, '3': 8, '4': 8, '5': 8, '6': 8, '7': 8}, {'a': 32, 'e': 32, 'i': 32, 'm': 32, 'q': 32, 'u': 32, 'y': 32, '4': 32}, {'=': 256}, {'=': 256}, {'=': 256}, {'=': 256}, {'=': 256}, {'=': 256}]

    fq2 = [{'a': 2048, 'b': 2048, 'c': 2048, 'd': 2048, 'e': 2048, 'f': 2048, 'g': 2048, 'h': 2048, 'i': 2048, 'j': 2048, 'k': 2048, 'l': 2048, 'm': 2048, 'n': 2048, 'o': 2048, 'p': 2048, 'q': 2048, 'r': 2048, 's': 2048, 't': 2048, 'u': 2048, 'v': 2048, 'w': 2048, 'x': 2048, 'y': 2048, 'z': 2048, '2': 2048, '3': 2048, '4': 2048, '5': 2048, '6': 2048, '7': 2048}, {'a': 2048, 'b': 2048, 'c': 2048, 'd': 2048, 'e': 2048, 'f': 2048, 'g': 2048, 'h': 2048, 'i': 2048, 'j': 2048, 'k': 2048, 'l': 2048, 'm': 2048, 'n': 2048, 'o': 2048, 'p': 2048, 'q': 2048, 'r': 2048, 's': 2048, 't': 2048, 'u': 2048, 'v': 2048, 'w': 2048, 'x': 2048, 'y': 2048, 'z': 2048, '2': 2048, '3': 2048, '4': 2048, '5': 2048, '6': 2048, '7': 2048}, {'a': 2048, 'b': 2048, 'c': 2048, 'd': 2048, 'e': 2048, 'f': 2048, 'g': 2048, 'h': 2048, 'i': 2048, 'j': 2048, 'k': 2048, 'l': 2048, 'm': 2048, 'n': 2048, 'o': 2048, 'p': 2048, 'q': 2048, 'r': 2048, 's': 2048, 't': 2048, 'u': 2048, 'v': 2048, 'w': 2048, 'x': 2048, 'y': 2048, 'z': 2048, '2': 2048, '3': 2048, '4': 2048, '5': 2048, '6': 2048, '7': 2048}, {'a': 32768, 'q': 32768}, {'=': 65536}, {'=': 65536}, {'=': 65536}, {'=': 65536}]

    fq3 = [{'a': 524288, 'b': 524288, 'c': 524288, 'd': 524288, 'e': 524288, 'f': 524288, 'g': 524288, 'h': 524288, 'i': 524288, 'j': 524288, 'k': 524288, 'l': 524288, 'm': 524288, 'n': 524288, 'o': 524288, 'p': 524288, 'q': 524288, 'r': 524288, 's': 524288, 't': 524288, 'u': 524288, 'v': 524288, 'w': 524288, 'x': 524288, 'y': 524288, 'z': 524288, '2': 524288, '3': 524288, '4': 524288, '5': 524288, '6': 524288, '7': 524288}, {'a': 524288, 'b': 524288, 'c': 524288, 'd': 524288, 'e': 524288, 'f': 524288, 'g': 524288, 'h': 524288, 'i': 524288, 'j': 524288, 'k': 524288, 'l': 524288, 'm': 524288, 'n': 524288, 'o': 524288, 'p': 524288, 'q': 524288, 'r': 524288, 's': 524288, 't': 524288, 'u': 524288, 'v': 524288, 'w': 524288, 'x': 524288, 'y': 524288, 'z': 524288, '2': 524288, '3': 524288, '4': 524288, '5': 524288, '6': 524288, '7': 524288}, {'a': 524288, 'b': 524288, 'c': 524288, 'd': 524288, 'e': 524288, 'f': 524288, 'g': 524288, 'h': 524288, 'i': 524288, 'j': 524288, 'k': 524288, 'l': 524288, 'm': 524288, 'n': 524288, 'o': 524288, 'p': 524288, 'q': 524288, 'r': 524288, 's': 524288, 't': 524288, 'u': 524288, 'v': 524288, 'w': 524288, 'x': 524288, 'y': 524288, 'z': 524288, '2': 524288, '3': 524288, '4': 524288, '5': 524288, '6': 524288, '7': 524288}, {'a': 524288, 'b': 524288, 'c': 524288, 'd': 524288, 'e': 524288, 'f': 524288, 'g': 524288, 'h': 524288, 'i': 524288, 'j': 524288, 'k': 524288, 'l': 524288, 'm': 524288, 'n': 524288, 'o': 524288, 'p': 524288, 'q': 524288, 'r': 524288, 's': 524288, 't': 524288, 'u': 524288, 'v': 524288, 'w': 524288, 'x': 524288, 'y': 524288, 'z': 524288, '2': 524288, '3': 524288, '4': 524288, '5': 524288, '6': 524288, '7': 524288}, {'a': 1048576, 'c': 1048576, 'e': 1048576, 'g': 1048576, 'i': 1048576, 'k': 1048576, 'm': 1048576, 'o': 1048576, 'q': 1048576, 's': 1048576, 'u': 1048576, 'w': 1048576, 'y': 1048576, '2': 1048576, '4': 1048576, '6': 1048576}, {'=': 16777216}, {'=': 16777216}, {'=': 16777216}]

    from fractions import *
    fq = fq3
    for i, dct in enumerate(fq):
        sm = sum(dct.values())
        for k, v in sorted(dct.items()):
            f = Fraction(v, sm)
            if f != Fraction(1, 32):
                print(i, k, v, f, f == Fraction(1, 32))
