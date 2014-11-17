#!/usr/bin/env python
# very ad hoc implementation of netcat
# for use on systems that don't have it installed  D:
import socket

def netcat(host, port, content):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    s.sendall(content)
    s.shutdown(socket.SHUT_WR)
    while True:
        data = s.recv(1024)
        if data == "":
            break
        print("received: {0}".format(repr(data)))
    print("connection closed")
    s.close()
