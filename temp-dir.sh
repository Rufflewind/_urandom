#!/bin/sh
wd=
unset wd
wd=`mktemp -d` || {
    printf >&2 "%s: can't make temp dir\n" "$0"
    exit 1
}
trap 'rm -fr "$wd"' 0 1 2 13 15
# do something with "$wd" ...