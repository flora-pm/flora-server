source environment.sh

export FLORA_DB_HOST="database"
export FLORA_HTTP_PORT=8084
export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE}\
  user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"
export PGPASSWORD=${FLORA_PG_PASSWORD}
