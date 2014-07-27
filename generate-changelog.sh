#!/bin/sh
# note, does not work on the very first tag :\
CURR_TAG=`git describe --abbrev=0 --tags`
PREV_TAG=`git describe --abbrev=0 --tags "$CURR_TAG"~1`
git log --oneline --pretty="  - %s" "$PREV_TAG".."$CURR_TAG"
