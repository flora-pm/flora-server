#!/usr/bin/env bash

sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
curl -s https://packagecloud.io/install/repositories/souffle-lang/souffle/script.deb.sh | sudo bash
sudo apt -y update
sudo apt -y install postgresql-14 souffle g++

