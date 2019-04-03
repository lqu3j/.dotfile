#!/bin/sh

userresources=$HOME/.Xresource

# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi
$HOME/.local/bin/autostart
xrandr --output DP1 --right-of HDMI2
nm-applet > /dev/null 2>&1 &
exec i3 > /dev/null 2>&1
