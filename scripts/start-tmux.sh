#!/usr/bin/env bash

set -euxo pipefail

tmux kill-session -t 'flora' || true
tmux new-session -d -s 'flora'
tmux rename-window 'flora'
sleep 1
tmux send-keys -t "flora" 'nix-shell --run "make nix-start"' 'C-m'
tmux select-window -t flora:0
sleep 1
tmux split-window -h 'nix-shell --run "make assets-watch"'
printf "To see the session, run: \n"
printf "$ tmux attach-session -t flora\n"
