#!/usr/bin/env python
#
# This is an interactive encryption and decryption script for the RSA cipher.
#
# Requires:
#
#   - Python (either 2 or 3)
#   - PyCrypto
#
import base64, os, sys, traceback
from Crypto.Cipher import PKCS1_OAEP
from Crypto.PublicKey import RSA

def generate_key():
    key = RSA.generate(4096)
    pubkey_str = key.publickey().exportKey().decode('utf-8')
    return key, pubkey_str

def encrypt(pubkey_str, message):
    pubkey = RSA.importKey(pubkey_str.encode('utf-8'))
    cipher = PKCS1_OAEP.new(pubkey)
    ciphertext_bytes = cipher.encrypt(message)
    ciphertext = base64.b64encode(ciphertext_bytes).decode('utf-8')
    return ciphertext

def decrypt(key, ciphertext):
    ciphertext = ''.join(ciphertext.split())
    ciphertext_bytes = base64.b64decode(ciphertext.encode('utf-8'))
    cipher = PKCS1_OAEP.new(key)
    message = cipher.decrypt(ciphertext_bytes)
    return message

def find_unused_filename(filename_format):
    i = 0
    while os.path.exists(filename_format.format(i)):
        i += 1
    return os.path.abspath(filename_format.format(i))

def read_file(filename, text=True):
    if text:
        flags = 'rt'
    else:
        flags = 'rb'
    with open(filename, flags) as f:
        return f.read()

def write_file(filename, data, text=True):
    if text:
        flags = 'wt'
    else:
        flags = 'wb'
    with open(filename, flags) as f:
        f.write(data)

def read_command():
    s = sys.stdin.readline()
    if not s:
        raise KeyboardInterrupt
    return s

def main_recv():
    sys.stdout.write('\nGenerating key... ')
    sys.stdout.flush()
    key, pubkey_str = generate_key()
    sys.stdout.write('done.\n')
    pubkey_filename     = find_unused_filename('public-key-{0}.txt')
    ciphertext_filename = find_unused_filename('encrypted-message-{0}.txt')
    write_file(pubkey_filename, pubkey_str)
    sys.stdout.write('''

The public key has been saved to:

    {0}

Email this key file to the sender.  Do not close this window
until the sender has replied with the encrypted message, otherwise
you'll have to start all over again.

When the sender has replied to your message,
save the encrypted message to a plain text file in:

    {1}

and then press ENTER.

> '''[1:].format(pubkey_filename, ciphertext_filename))
    sys.stdout.flush()
    while True:
        read_command()
        try:
            ciphertext = read_file(ciphertext_filename)
        except IOError:
            sys.stdout.flush()
            sys.stdout.write('''

Can't read the encrypted message file.  (Does it exist?)
To try again, press ENTER.

> '''[1:])
            sys.stdout.flush()
            continue
        sys.stdout.write('\nDecrypting... ')
        sys.stdout.flush()
        try:
            message = decrypt(key, ciphertext)
        except Exception:
            sys.stdout.flush()
            sys.stdout.write('''

Decryption error.
To try again, press ENTER.

> ''')
            sys.stdout.flush()
            continue
        break

    sys.stdout.write('done.\n')
    message_filename = find_unused_filename('decrypted-message-{0}.txt')
    write_file(message_filename, message, text=False)
    sys.stdout.write('''

The decrypted message has been saved to:

    {0}

'''[1:].format(message_filename))

def main_send():
    pubkey_filename  = find_unused_filename('public-key-{0}.txt')
    message_filename = find_unused_filename('message-{0}.txt')
    sys.stdout.write('''

 1. Request the public key from the recipent and save the
    key file to:

        {0}

 2. Write your message and save it to a file named:

        {1}

When you are done with both steps 1 and 2, press ENTER.

> '''[1:].format(pubkey_filename, message_filename))
    while True:
        read_command()
        try:
            message = read_file(message_filename, text=False)
        except IOError:
            sys.stdout.flush()
            sys.stdout.write('''

Can't read the message file.  (Does it exist?)
To try again, press ENTER.

> '''[1:])
            continue
        try:
            pubkey_str = read_file(pubkey_filename)
        except IOError:
            sys.stdout.flush()
            sys.stdout.write('''

Can't read the key file.  (Does it exist?)
To try again, press ENTER.

> '''[1:])
            continue
        sys.stdout.write('\nEncrypting... ')
        sys.stdout.flush()
        try:
            ciphertext = encrypt(pubkey_str, message)
        except Exception:
            sys.stdout.flush()
            sys.stdout.write('''

Encryption error.  Is the public key valid?
To try again, press ENTER.

> ''')
            continue
        break
    sys.stdout.write('done.\n')
    ciphertext_filename = find_unused_filename('encrypted-message-{0}.txt')
    write_file(ciphertext_filename, ciphertext)
    sys.stdout.write('''

The encrypted message has been saved to:

    {0}

Now send this encrypted file to your intended recipient.

'''[1:].format(ciphertext_filename))

def main():
    while True:
        sys.stdout.write(
            'Do you want to send [S] or receive [R] a message?\n' +
            'Type either S or R followed by ENTER.\n\n> ')
        sys.stdout.flush()
        line = read_command()
        if not line:                    # EOF
            sys.stdout.write('\n')
            break
        command = line.strip().lower()
        if command.startswith('s'):
            main_send()
            break
        elif command.startswith('r'):
            main_recv()
            break
        elif command.startswith('q'):
            sys.stdout.write('Quitting ...\n')
            break
        elif not command:
            sys.stdout.write('Error: no command provided.\n')
            sys.stdout.flush()
        else:
            sys.stdout.write('Error: unknown command: ' + command + '\n')
            sys.stdout.flush()

try:
    main()
except KeyboardInterrupt:
    sys.stdout.write('\n')
    sys.exit(2)
except Exception:
    sys.stdout.write('\nError:\n')
    traceback.print_exc(file=sys.stdout)
    sys.stdout.write('Press ENTER to exit.\n> ')
    sys.stdout.flush()
    read_command()
    sys.exit(1)
