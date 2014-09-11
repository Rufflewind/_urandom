#!/bin/sh
#
# Usage: shrink.sh IN OUT
#
# This shell script uses ImageMagick to shrink PDFs.  Feel free to tweak the settings below.
#
# By Rufflewind.  No rights reserved (public domain / CC0).
# https://gist.github.com/Rufflewind/cab8c5ed57868cb74e72

set -e

IN="$1"
OUT="$2"

if [ -z "$IN" ] || [ -z "$OUT" ]
then
    echo >&2 "Usage: shrink.sh IN OUT"
    exit 1
fi

convert "$IN" "$IN"-tmp.jpg
for FILE in "$IN"-tmp-*.jpg
do
    convert \
        -strip -interlace Plane \
        -quality 75% \
        -resize 50% \
        "$FILE" "$FILE".jpg
        # -sampling-factor 4:2:0 \
        # -gaussian-blur 0.05 \
    rm "$FILE"
done
convert "$IN"-tmp-*.jpg.jpg "$OUT"
rm "$IN"-tmp-*.jpg.jpg
ls -ahl "$IN" "$OUT"
