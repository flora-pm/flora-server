# this is a pinned ubuntu:22.04 (newer versions have incomptible
# library versions for souffle)
FROM ubuntu@sha256:67211c14fa74f070d27cc59d69a7fa9aeff8e28ea118ef3babc295a0428a6d21

ARG GID=1000
ARG UID=1000

ARG ghc_version=9.4.8
ARG cabal_version=3.10.2.0

# generate a working directory
USER "root"
ARG USER="local"
RUN groupadd -g "$GID" -o "$USER" \
    && useradd -r -u "$UID" -g "$GID" -m -s /bin/zsh "$USER"

# We create the folder explicitly so that we can give nonprivileged user the appropriate access
RUN mkdir /flora-server
RUN chown $USER:$USER /flora-server

RUN mkdir /home/$USER/.cabal
RUN chown -R $USER:$USER /home/$USER/.cabal
WORKDIR /flora-server 

RUN apt update && \
    apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 git

# install dependencies (pg_config, postgresql-client, yarn)
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE="YES"
ENV BOOTSTRAP_HASKELL_GHC_VERSION="$ghc_version"
ENV BOOTSTRAP_HASKELL_CABAL_VERSION="$cabal_version"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_HLS="YES"
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list 
RUN apt install -y nodejs libpq-dev mcpp wget zsh tmux postgresql-client
RUN corepack enable
RUN chmod ugo+x /home/$USER/.cabal

USER ${USER}
RUN git config --global --add safe.directory "*"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


USER ${USER}
ENV PATH="$PATH:/home/$USER/.ghcup/bin"

# install soufflé
USER "root"
RUN wget --content-disposition https://github.com/souffle-lang/souffle/releases/download/2.3/x86_64-ubuntu-2004-souffle-2.3-Linux.deb
RUN apt install -f -y ./x86_64-ubuntu-2004-souffle-2.3-Linux.deb
USER ${USER}

RUN echo $PATH

# install Haskell tooling (note that for cabal, it's probably better
# to run `cabal update` as separate step, as cabal doesn't delete
# package versions)
RUN cabal update
RUN cabal install -j postgresql-migration 
RUN cabal install -j hlint apply-refact
RUN cabal install -j fourmolu-0.14.1.0
RUN cabal install -j cabal-fmt
RUN cabal install -j ghcid
RUN cabal install -j ghc-tags

# configure the shell
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
COPY --chown=${USER} scripts/shell-welcome.txt /etc/motd
COPY --chown=${USER} scripts/.zshrc /home/$USER/.zshrc

# build Haskell dependencies
COPY --chown=${USER} cabal.project flora.cabal ./
RUN cabal build --only-dependencies -j

# compile Souffle source files
COPY --chown=${USER} Makefile ./
COPY --chown=${USER} cbits ./cbits
RUN make souffle

# copy and build the assets
COPY --chown=${USER} assets ./assets
COPY --chown=${USER} docs ./docs
RUN make build-assets

USER root
RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' > /etc/profile
USER ${USER}
