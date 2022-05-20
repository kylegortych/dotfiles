#!/bin/sh

# note: add xterm escape sequences to neo.txt | font size and color
column -t -s '|' < ~/CLI\ Support/sh\ scripts/info.sh
date +"-- %a %m-%d-%Y %I:%M%p --"
printf '\n'
ps
