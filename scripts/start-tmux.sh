set -euxo pipefail

tmux kill-session -t 'flora' || true

LEFT_PANE_CMD='make watch-server'
RIGHT_PANE_CMD="(cd assets && yarn install); make watch-assets"

tmux \
  new-session -d -s 'flora' -n 'flora' \; \
  split-window -t 'flora' -h \; \
  send-keys -t "flora:flora.0" "$LEFT_PANE_CMD" 'C-m' \; \
  send-keys -t "flora:flora.1" "$RIGHT_PANE_CMD" 'C-m' \; \
  select-pane -t 'flora:flora.0'

tmux attach-session -t flora
