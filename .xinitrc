#!/bin/bash

FONTS_DIR="/home/woocat/.local/share/fonts"
for name in `ls ${FONTS_DIR}`;
do 
    dir=${FONTS_DIR}/${name}
    if [  -d  "$dir"  ]; then
        cd "$dir"
        xset +fp "$PWD"
        mkfontscale
        mkfontdir
        cd -
    fi
done
xset fp rehash

userresources=$HOME/.Xresource
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

source ~/.zshenv
exec i3 > /dev/null 2>&1 
