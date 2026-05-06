export FLORA_DB_PORT=5432
export FLORA_DB_HOST="flora-database"
export FLORA_DB_DATABASE="flora_dev_1"
export FLORA_HTTP_PORT=8084
export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} port=${FLORA_DB_PORT} dbname=${FLORA_DB_DATABASE}\
  user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"
export PGPASSWORD=${FLORA_DB_PASSWORD}

export FLORA_PG_URI="postgresql://${FLORA_DB_USER}:${FLORA_DB_PASSWORD}@${FLORA_DB_HOST}:${FLORA_DB_PORT}/${FLORA_DB_DATABASE}"

export FLORA_PROMETHEUS_ENABLED="true" # Set this variable to true or false to enable Prometheus metrics export
export FLORA_JOB_HTTP_PORT="8085"
