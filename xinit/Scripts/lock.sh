#!/usr/bin/env bash

LOCKFILE=/tmp/lockscreen.png

scrot -o $LOCKFILE
size=$(identify -format "%[fx:w]x%[fx:h]" "$LOCKFILE")

convert $LOCKFILE -scale 70 -sample $size\! -quality 11 $LOCKFILE

# i3lock -i $LOCKFILE

