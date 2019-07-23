#!/bin/bash
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

export GO111MODULE=on
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:~/.local/bin:~/.fzf/bin/:$HOME/.cargo/bin:$HOME/.local/apache-maven-3.6.1/bin
