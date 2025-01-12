#!/usr/bin/env bash

sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
sudo apt -y update
sudo apt -y install postgresql-16 g++ mcpp libffi7 llvm zlib1g-dev

wget https://github.com/souffle-lang/souffle/releases/download/2.2/x86_64-ubuntu-2004-souffle-2.2-Linux.deb
sudo dpkg -i ./x86_64-ubuntu-2004-souffle-2.2-Linux.deb
