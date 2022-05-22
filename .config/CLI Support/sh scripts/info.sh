#!/bin/sh

# note: add xterm escape sequences to neo.txt | font size and color
column -t -s '|' < ~/.config/CLI\ Support/aliase\ \&\ script\ support/neo.txt 
date +"-- %a %m-%d-%Y %I:%M%p --"
printf '\n'
ps
