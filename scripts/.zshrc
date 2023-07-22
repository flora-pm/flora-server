export ZSH="$HOME/.oh-my-zsh"
export LANG=C.UTF-8
ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source /flora-server/environment.docker.sh

cat /etc/motd
