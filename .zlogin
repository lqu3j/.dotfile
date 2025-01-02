#!/bin/bash
export GO111MODULE=on
export GOPATH=$HOME/.go
export GOPROXY="https://goproxy.io,direct"
export GOPRIVATE=*.zx-tech.net
export GOSUMDB=off
export CGO_ENABLED=0
export GOINSECURE="git.algor.tech"
export PATH=$PATH:$GOPATH/bin:~/.local/bin:~/.fzf/bin/:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.local/bin:$DENO_INSTALL/bin:~/.cargo/bin:$PATH
export FZF_DEFAULT_OPTS="--layout=reverse --inline-info --color=gutter:#889901,bg+:6" 
export TERM=xterm-256color
export EDITOR=nvim
export NALI_DB_IP4=ip2location
export NALI_DB_IP6=ip2location
export LIBVIRT_DEFAULT_URI=qemu:///system


