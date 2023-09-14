#!/usr/bin/env zsh

set -o pipefail

export SHELL="zsh"
export ZSH="$HOME/.oh-my-zsh"
export LANG=C.UTF-8
ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source /flora-server/environment.docker.sh
export PATH=${PATH}:~/.cabal/bin

cat /etc/motd
