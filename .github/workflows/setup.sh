#!/usr/bin/env bash

sudo install -d /usr/share/postgresql-common/pgdg
sudo curl --fail --silent --show-error -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc https://www.postgresql.org/media/keys/ACCC4CF8.asc
sudo sh -c 'echo "deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
sudo apt -y update
sudo apt -y install postgresql-17 g++ mcpp libffi8 llvm zlib1g-dev libsodium-dev liburing-dev
