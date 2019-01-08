#!/bin/sh

userresources=$HOME/.Xresource

# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

#export LC_CTYPE=zh_CN.UTF-8
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

pkill ssh-agent
eval $(ssh-agent)

xsetroot -cursor_name left_ptr
xset +fp /home/woocat/.local/share/fonts/FontAwesome
xset +fp /home/woocat/.local/share/fonts/InconsolataGo
xset +fp /home/woocat/.local/share/fonts/Termsyn
xset +fp /home/woocat/.local/share/fonts/ZhunYuan
xset +fp /home/woocat/.local/share/fonts/Monofur\ for\ Powerline
xset +fp /home/woocat/.local/share/fonts/Source\ Code\ Pro\ for\ Powerline
/home/woocat/.local/bin/kcptun_client -r "104.225.147.158:4000" -l ":8388" --rcvwnd 4096 --sndwnd 4096  --mode  fast3 --quiet > /dev/null 2>&1 &
exec bspwm
