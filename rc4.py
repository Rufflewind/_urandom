# RC4 stream cipher in Python
# author: Ninji Vahran (Treeki)

def rc4_keystream(key):
    S = list(range(256))
    j = 0
    for i in range(256):
        j = (j + S[i] + key[i % len(key)]) & 0xFF
        S[i], S[j] = S[j], S[i]

    i = 0
    j = 0
    while True:
        i = (i + 1) & 0xFF
        j = (j + S[i]) & 0xFF
        S[i], S[j] = S[j], S[i]
        yield S[(S[i] + S[j]) & 0xFF]

def rc4_data(key, data):
    data = bytearray(data)
    for i, xor in zip(range(len(data)), rc4_keystream(key)):
        data[i] = data[i] ^ xor
    return bytes(data)
