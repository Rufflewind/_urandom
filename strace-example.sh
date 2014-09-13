#!/bin/sh
strace 2>tmp.log -T "$program"
sed 's/\(.*\)<\(.*\)>/\2\t\1/' tmp.log | sort >tmp2.log
