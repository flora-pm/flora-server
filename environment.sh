export DB_HOST="localhost"
export DB_PORT="5432"
export DB_USER="postgres"
export DB_PASSWORD="postgres"
export DB_DATABASE="flora_dev"
export DB_POOL_CONNECTIONS="10"
export DB_SUB_POOLS="10"
export DB_TIMEOUT="10"

export FLORA_DB_URI="postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_DATABASE}?sslmode=disable"

export DBMATE_MIGRATIONS_DIR="./database/migrations"
export DBMATE_SCHEMA_FILE="./database/schema.sql"
export FLORA_PORT=8083

export FLORA_ENVIRONMENT="local"
#export SENTRY_DSN="" # Set this variable in `environment.local.sh`, which is not tracked by git.
