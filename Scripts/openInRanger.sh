#!/bin/bash
$TERM -e ranger $( echo ~/*/ | sed "s/ /\n/g" | cat .bookmarks - | dmenu -p Dir -l 10 -i | sed "s:~:${HOME}:")
