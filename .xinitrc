#!/bin/sh

userresources=$HOME/.Xresource

# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

$HOME/.local/bin/autostart
exec bspwm > /dev/null 2>&1 
