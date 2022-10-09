#!/usr/bin/env bash

LOCKFILE=/tmp/lockscreen.png
INFILE=$HOME/Pictures/current_wall

convert $INFILE -blur 10x10 $LOCKFILE

i3lock -i $LOCKFILE

