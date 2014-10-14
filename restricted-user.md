Restricted user for port forwarding only
========================================

Add the user and disable the login shell:

    # useradd -m -s /usr/bin/nologin USER

Copy the public key to the new user's `authorized_keys` and restrictions:

    # ~/.ssh/authorized_keys
    no-X11-forwarding,permitopen="localhost:PORT" ssh-rsa KEY COMMENT

