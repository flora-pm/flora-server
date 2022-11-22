# syntax=docker/dockerfile:1
FROM ubuntu:22.04

# generate a working directory
WORKDIR /flora-server 

RUN apt update
RUN apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo 'export PATH="$PATH:/root/.ghcup/bin"' >> /etc/profile
ENV PATH="$PATH:/root/.ghcup/bin"
RUN ghcup install ghc 9.2.4 && ghcup set ghc 9.2.4 && ghcup install cabal

# install dependencies (pg_config, postgresql-client, postrgresql-migrations & yarn)
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list 
RUN apt install -y nodejs libpq-dev mcpp wget zsh tmux postgresql-client
RUN corepack enable
RUN cabal update
RUN cabal install -j8 postgresql-migration

# install soufflé
RUN wget --content-disposition https://github.com/souffle-lang/souffle/releases/download/2.2/x86_64-ubuntu-2004-souffle-2.2-Linux.deb
RUN apt install -f -y ./x86_64-ubuntu-2004-souffle-2.2-Linux.deb

# configure the shell
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
COPY scripts/shell-welcome.txt /etc/motd
COPY scripts/.zshrc /root/.zshrc

# build Haskell dependencies
COPY Makefile cabal.project flora.cabal ./
RUN cabal build --only-dependencies -j

# Compile Souffle source files
COPY cbits ./cbits
RUN make souffle

# copy and build the assets
COPY assets ./assets
RUN make build-assets

RUN echo $PATH > /etc/profile
CMD [ "/bin/sh", "-c", "sleep 1d"]
