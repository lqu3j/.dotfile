#!/bin/bash
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export GO111MODULE=auto
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin:~/.local/bin:~/.fzf/bin/
export FZF_DEFAULT_OPTS="--layout=reverse --inline-info"
