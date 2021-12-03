source environment.sh

export DB_DATABASE="flora_test"
export FLORA_DB_URI="postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_DATABASE}?sslmode=disable"
