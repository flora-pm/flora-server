. environment.sh

export FLORA_HTTP_PORT=8085
export FLORA_ENVIRONMENT="tests"
export FLORA_DOMAIN="localhost"

export FLORA_DB_DATABASE="flora_test"

export FLORA_PG_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"
