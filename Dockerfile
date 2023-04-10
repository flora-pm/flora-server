# this is a pinned ubuntu:22.04 (newer versions have incomptible
# library versions for souffle)
FROM ubuntu@sha256:67211c14fa74f070d27cc59d69a7fa9aeff8e28ea118ef3babc295a0428a6d21

ARG ghc_version=9.4.4
ARG cabal_version=3.10.1.0

# generate a working directory
WORKDIR /flora-server 

RUN apt update && \
    apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo 'export PATH="$PATH:/root/.ghcup/bin"' >> /etc/profile
ENV PATH="$PATH:/root/.ghcup/bin"
RUN ghcup install ghc $ghc_version && \
    ghcup set ghc $ghc_version && \
    ghcup install cabal $cabal_version

# install dependencies (pg_config, postgresql-client, yarn)
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list 
RUN apt install -y nodejs libpq-dev mcpp wget zsh tmux postgresql-client
RUN corepack enable

# install soufflé
RUN wget --content-disposition https://github.com/souffle-lang/souffle/releases/download/2.3/x86_64-ubuntu-2004-souffle-2.3-Linux.deb
RUN apt install -f -y ./x86_64-ubuntu-2004-souffle-2.3-Linux.deb

# install Haskell tooling (note that for cabal, it's probably better
# to run `cabal update` as separate step, as cabal doesn't delete
# package versions)
RUN cabal update
RUN cabal install -j8 postgresql-migration ghcid

# configure the shell
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
COPY scripts/shell-welcome.txt /etc/motd
COPY scripts/.zshrc /root/.zshrc

# build Haskell dependencies
COPY cabal.project flora.cabal ./
RUN cabal build --only-dependencies -j

# compile Souffle source files
COPY Makefile ./
COPY cbits ./cbits
RUN make souffle

# copy and build the assets
COPY assets ./assets
RUN make build-assets

ENV PATH="$PATH:/root/.cabal/bin"
RUN echo $PATH > /etc/profile
CMD [ "/bin/sh", "-c", "sleep 1d"]
