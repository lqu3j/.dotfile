#!/bin/bash

FONTS_DIR="/home/woocat/.local/share/fonts"
for name in `ls ${FONTS_DIR}`;
do 
    dir=${FONTS_DIR}/${name}
    if [  -d  "$dir"  ]; then
        cd "$dir"
        xset +fp "$PWD" > /dev/null 2>&1
        mkfontscale > /dev/null 2>&1
        mkfontdir  > /dev/null 2>&1
        cd -  > /dev/null 2>&1
    fi
done
xset fp rehash >  /dev/null 2>&1

userresources=$HOME/.Xresource
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

source ~/.zshenv
exec i3 > ~/.log/i3.log 2>&1
