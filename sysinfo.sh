#!/bin/sh

# general system info
hwinfo --short

# size of memory
cat /proc/meminfo | grep MemTotal

# type of memory
sudo dmidecode --type 17
