export ZSH="$HOME/.oh-my-zsh"
export LANG=C.UTF-8
ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source /flora-server/environment.sh
export FLORA_DB_HOST="database"
export FLORA_PG_URI="postgresql://${FLORA_DB_USER}:${FLORA_DB_PASSWORD}@${FLORA_DB_HOST}:${FLORA_DB_PORT}/${FLORA_DB_DATABASE}"
export FLORA_PG_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"
export FLORA_HTTP_PORT=8084

cat /etc/motd
