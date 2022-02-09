# syntax=docker/dockerfile:1
FROM haskell:8.10

# generate a working directory
WORKDIR /flora-server 

# copy the files relevant to build core dependencies
COPY cabal.project flora.cabal shell.nix environment.sh environment.docker.sh Makefile scripts/start-tmux.sh ./

RUN cabal update
# let nix build the dependencies. This uses nix-shell to cache the setup phase.
RUN cabal build -j4

# copy asset-relevant dependency files
COPY assets/package.json assets/yarn.lock assets/
RUN make assets-deps

CMD [ "/bin/sh", "-c", "sleep 1d"]
