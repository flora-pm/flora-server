# syntax=docker/dockerfile:1
FROM nixos/nix

RUN nix-channel --update
RUN nix-env -iA nixpkgs.gnumake

# generate a working directory
WORKDIR /flora-server 

# copy the files relevant to build core dependencies
COPY default.nix flora.cabal shell.nix environment.sh environment.docker.sh Makefile scripts/start-tmux.sh ./
COPY nix/ ./nix/

# let nix build the dependencies. This uses nix-shell to cache the setup phase.
RUN nix-shell

# copy asset-relevant dependency files
COPY assets/package.json assets/yarn.lock assets/
RUN nix-shell --run "make assets-deps"

CMD [ "/bin/sh", "-c", "sleep 1d"]
