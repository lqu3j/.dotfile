#!/bin/bash

# FONTS_DIR="/home/lx/.local/share/fonts"
# for name in `ls ${FONTS_DIR}`;
# do 
#     dir=${FONTS_DIR}/${name}
#     if [  -d  "$dir"  ]; then
#         cd "$dir"
#         xset +fp "$PWD" > /dev/null 2>&1
#         mkfontscale > /dev/null 2>&1
#         mkfontdir  > /dev/null 2>&1
#         cd -  > /dev/null 2>&1
#     fi
# done
# xset fp rehash >  /dev/null 2>&1
# xsetroot -cursor_name left_ptr &
# 
# userresources=$HOME/.Xresource
# # merge in defaults and keymaps
# if [ -f "$userresources" ]; then
#     xrdb -merge "$userresources"
# fi
# 
source ~/.zshenv
exec dbus-launch --exit-with-session i3 
#exec dbs-launch --exit-with-session startplasma-x11
#exec startplasma-x11
