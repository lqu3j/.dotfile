#!/bin/bash
HOST=`hostname`

# start ssh-agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
        ssh-agent | grep -v echo> ~/.ssh-agent-thing
fi
source ~/.ssh-agent-thing
# ssh-add key
DIR=${HOME}/.ssh/private
ssh-add ${DIR}/*

if [ "$HOST" = "lazzy-company-pc"  ];then
    xrandr --output DP-1 --mode 1920x1080 --rate 60.0 --right-of HDMI-2  --mode 1920x1080 --rate 60.0
fi
