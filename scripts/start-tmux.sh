#!/usr/bin/env bash

set -euxo pipefail

tmux kill-session -t 'flora' || true
tmux new-session -d -s 'flora'
tmux rename-window 'flora'
sleep 1
tmux send-keys -t "flora" 'make ghcid-server' 'C-m'
tmux select-window -t flora:0
sleep 1
tmux split-window -h 'make assets-watch'
tmux send-keys -t "flora" 'C-b'
tmux attach-session -t flora
