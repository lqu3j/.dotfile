#!/bin/sh

userresources=$HOME/.Xresource

# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

$HOME/.local/bin/autostart

HOST=`hostname`
if [ "$HOST" = "lazzy-company-pc" ];then
    xrandr --output DP1 --right-of HDMI2
fi

exec i3 > /dev/null 2>&1
