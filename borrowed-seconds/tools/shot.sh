#!/bin/bash
# Development helper, macOS only: open the game frozen at a moment and photograph it.
#   tools/shot.sh <drive-binary> <out.png> [game flags...]
drive=$1; out=$2; shift 2
bin=$(cabal list-bin exe:borrowed-seconds)
"$bin" "$@" >/dev/null 2>&1 &
pid=$!
for _ in 1 2 3 4 5 6 7 8 9 10; do
  sleep 0.6
  wid=$("$drive" winid "$pid") && break
done
sleep 0.5
screencapture -x -o -l"$wid" "$out"
kill "$pid"
