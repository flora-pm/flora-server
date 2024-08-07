ARG HLS_VERSION=2.9.0.1
ARG GHC_VERSION=9.6.6
ARG CABAL_VERSION=3.12.1.0

ARG APPLY_REFACT_VERSION=0.14.0.0
ARG CABAL_FMT_VERSION=0.1.12
ARG FOURMOLU_VERSION=0.16.2.0
ARG GHCID_VERSION=0.8.9
ARG GHC_TAGS_VERSION=1.8
ARG HLINT_VERSION=3.8
ARG POSTGRESQL_MIGRATION_VERSION=0.2.1.8

# this is a pinned ubuntu:22.04 (newer versions have incomptible
# library versions for souffle)
FROM ubuntu@sha256:67211c14fa74f070d27cc59d69a7fa9aeff8e28ea118ef3babc295a0428a6d21 AS base
RUN apt update && \
    apt install -y build-essential curl libffi-dev libffi8 libgmp-dev \
        libgmp10 libncurses-dev libncurses5 libtinfo5 git libsodium-dev \
        pkg-config wget 

FROM base AS node-setup
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list 
RUN apt install -y nodejs
RUN corepack enable

FROM node-setup AS assets-build
# copy and build the assets
COPY --chown=${USER} assets ./assets
COPY --chown=${USER} docs ./docs
RUN make build-assets

FROM base AS ghcup
ARG GHC_VERSION
ARG CABAL_VERSION
ENV PATH="/opt/ghcup/bin:$PATH" \
    GHCUP_INSTALL_BASE_PREFIX="/opt/ghc/"
RUN mkdir -p /opt/ghcup/bin/ \
    && export DOWNLOADARCH=x86_64 \
    && if [ "$TARGETARCH" = "arm64" ]; then export DOWNLOADARCH=aarch64; fi \
    && curl -L https://downloads.haskell.org/~ghcup/$DOWNLOADARCH-linux-ghcup --output /opt/ghcup/bin/ghcup \
    && chmod +x /opt/ghcup/bin/ghcup \
    && ghcup install ghc $GHC_VERSION \
    && ghcup set ghc $GHC_VERSION \
    && ghcup install cabal $CABAL_VERSION
ENV PATH="/opt/ghc/.ghcup/bin/:$PATH"

FROM base AS souffle
# install soufflé
USER "root"
RUN apt install -y mcpp
RUN wget --content-disposition https://github.com/souffle-lang/souffle/releases/download/2.2/x86_64-ubuntu-2004-souffle-2.2-Linux.deb
RUN apt install -f -y ./x86_64-ubuntu-2004-souffle-2.2-Linux.deb

FROM ghcup AS haskell-tools
ARG GHC_VERSION
ARG APPLY_REFACT_VERSION
ARG CABAL_FMT_VERSION
ARG CABAL_VERSION
ARG FOURMOLU_VERSION
ARG GHCID_VERSION
ARG GHC_TAGS_VERSION
ARG HLINT_VERSION
ARG POSTGRESQL_MIGRATION_VERSION
RUN ghcup set ghc $GHC_VERSION
RUN cd /tmp \
    && cabal update \
    && cabal install -j --install-method=copy --installdir=/out \ 
        apply-refact-$APPLY_REFACT_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \ 
        cabal-fmt-$CABAL_FMT_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \
        fourmolu-$FOURMOLU_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \
        ghcid-$GHCID_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \
        ghc-tags-$GHC_TAGS_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \
        hlint-$HLINT_VERSION \
    && cabal install -j --install-method=copy --installdir=/out \
        postgresql-migration-$POSTGRESQL_MIGRATION_VERSION

FROM base AS shell-config
# configure the shell
USER ${USER}
RUN apt install -y zsh tmux
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
COPY --chown=${USER} scripts/shell-welcome.txt /etc/motd
COPY --chown=${USER} scripts/.zshrc /home/$USER/.zshrc

FROM ghcup AS server
ENV PATH="/opt/ghcup/bin:/opt/ghc/.ghcup/bin/:$PATH"
COPY --from=ghcup /opt/ghcup/ /opt/ghcup/
COPY --from=ghcup /opt/ghc/ /opt/ghc/
COPY --from=haskell-tools /out/* /usr/local/bin/
# build Haskell dependencies
COPY --chown=${USER} cabal.project flora.cabal cabal.project.freeze ./
RUN cabal build --only-dependencies -j
# compile Souffle source files
COPY --chown=${USER} Makefile ./
COPY --chown=${USER} cbits ./cbits
RUN make souffle
USER root
RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' > /etc/profile
USER ${USER}

FROM base AS devel
ARG GID=1000
ARG UID=1000
# generate a working directory
ARG HLS_VERSION
ARG USER="local"
COPY --from=ghcup /opt/ghcup/ /opt/ghcup/
COPY --from=ghcup /opt/ghc/ /opt/ghc/
COPY --from=haskell-tools /out/* /usr/local/bin/
RUN groupadd -g "$GID" -o "$USER" \
    && useradd -r -u "$UID" -g "$GID" -m -s /bin/zsh "$USER"
ENV GHCUP_INSTALL_BASE_PREFIX="/opt/ghc/"
RUN ghcup install hls $HLS_VERSION \
    && ghcup set hls $HLS_VERSION
# We create the folder explicitly so that we can give nonprivileged user the appropriate access
RUN mkdir /flora-server
RUN chown $USER:$USER /flora-server
RUN mkdir /home/$USER/.cabal
RUN chown -R $USER:$USER /home/$USER/.cabal
WORKDIR /flora-server 
