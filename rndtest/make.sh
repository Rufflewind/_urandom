#!/bin/sh
set -eu
cc -fPIC -shared -o librndmod.so rndmod.c
c++ -fPIC -I/usr/include/qt -I/usr/include/qt/QtCore -o rndtest rndtest.cpp -ldl -lQt5Core
./rndtest
