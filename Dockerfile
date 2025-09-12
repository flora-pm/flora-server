ARG APPLY_REFACT_VERSION=0.15.0.0
ARG BASE_IMAGE_VERSION=22.04
ARG CABAL_VERSION=3.14.1.1
ARG FOURMOLU_VERSION=0.18.0.0
ARG GHCID_VERSION=0.8.9
ARG GHC_TAGS_VERSION=1.9
ARG GHC_VERSION=9.10.1
ARG HLINT_VERSION=3.10
ARG HLS_VERSION=2.11.0.0
ARG POSTGRESQL_MIGRATION_VERSION=0.2.1.8

# This stage installs libraries required to install GHC and other tools
FROM ubuntu:$BASE_IMAGE_VERSION AS base
USER "root"
ENV LANG="en_GB.UTF-8"

RUN apt update \
    && apt install -y \
        build-essential \
        curl \
        git \
        libffi-dev \
        libffi8 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libpq-dev \
        libsodium-dev \
        libtinfo5 \
        locales \
        pkg-config \
        postgresql-client \
        zlib1g-dev
RUN localedef -i en_GB -c -f UTF-8 -A /usr/share/locale/locale.alias en_GB.UTF-8
RUN rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man

# This stage installs ghcup, ghc and cabal
FROM base AS ghcup
ARG CABAL_VERSION

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE="YES"
ENV BOOTSTRAP_HASKELL_MINIMAL="YES"
ENV GHCUP_INSTALL_BASE_PREFIX="/out/ghcup"
ENV PATH="/out/ghcup/.ghcup/bin:$PATH"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN ghcup install cabal $CABAL_VERSION

# This stage installs haskell tools
FROM base AS setup-haskell-tools
ARG APPLY_REFACT_VERSION
ARG FOURMOLU_VERSION
ARG GHCID_VERSION
ARG GHC_TAGS_VERSION
ARG GHC_VERSION
ARG HLINT_VERSION
ARG POSTGRESQL_MIGRATION_VERSION

ENV PATH="/opt/ghcup/.ghcup/bin:$PATH"
ENV GHCUP_INSTALL_BASE_PREFIX="/opt/ghcup"

COPY --from=ghcup /out/ghcup /opt/ghcup

RUN ghcup install ghc $GHC_VERSION
RUN ghcup install ghc 9.6.7
RUN ghcup set ghc $GHC_VERSION

RUN cabal update

RUN cabal install --install-method=copy --installdir=out/ --semaphore -j postgresql-migration-$POSTGRESQL_MIGRATION_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j fourmolu-$FOURMOLU_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j hlint-$HLINT_VERSION
RUN ghcup run --ghc 9.6.7 -- cabal install --install-method=copy --installdir=out/ -j apply-refact-$APPLY_REFACT_VERSION
RUN ghcup rm ghc 9.6.7
RUN ghcup gc -t -p -s -c
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j ghc-tags-$GHC_TAGS_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j ghcid-$GHCID_VERSION

# This stage is the development environment
FROM base AS devel
USER root

ARG GID
ARG UID
ARG USER
ENV USER=$USER

COPY --from=setup-haskell-tools /out /opt/bin
COPY --from=setup-haskell-tools /opt/ghcup /opt/ghcup

ENV PATH="/opt/ghcup/.ghcup/bin:/opt/bin:$PATH"

RUN ghcup install ghc $GHC_VERSION
RUN ghcup set ghc $GHC_VERSION

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt update
RUN apt install -y direnv \
  libpq-dev \
  wget \
  nodejs \
  postgresql-client \
  tmux \
  wget \
  zsh
RUN corepack enable
RUN git config --global --add safe.directory "*"

RUN groupadd -g "$GID" -o "$USER" \
    && useradd -l -r -u "$UID" -g "$GID" -m -s /bin/bash "$USER"

RUN mkdir /home/$USER/.cabal
RUN chmod ugo+x /home/$USER/.cabal
RUN chown -R $UID:$GID /home/$USER

USER $USER

# configure the shell
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
COPY --chown=${USER} scripts/shell-welcome.txt /etc/motd
COPY --chown=${USER} scripts/.zshrc /home/$USER/.zshrc

RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' >>~/.bashrc
RUN echo "source /opt/ghcup/.ghcup/env" >>~/.bashrc
RUN echo 'eval "$(direnv hook bash)"' >>~/.bashrc
RUN echo 'direnv allow' >>~/.bashrc
RUN cabal update

WORKDIR /flora-server
