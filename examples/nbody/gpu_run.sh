#!/usr/bin/env bash
# This bounds the host process, not the lifetime of an already submitted kernel.
set -euo pipefail

timeout=${GPU_TIMEOUT:-20}
maxmem=${GPU_MAXMEM_MB:-4096}
if ! [[ $timeout =~ ^[0-9]+$ && $maxmem =~ ^[0-9]+$ ]] ||
    (( timeout < 1 || timeout > 30 || maxmem < 1 )); then
    echo "GPU_TIMEOUT must be 1..30 seconds; GPU_MAXMEM_MB must be positive" >&2
    exit 2
fi
if (( $# == 0 )); then
    echo "usage: bash gpu_run.sh program [args...]" >&2
    exit 2
fi

lock=/tmp/gpurun.lock.d
acquired=false
for ((i = 0; i < 300; i++)); do
    if mkdir "$lock" 2>/dev/null; then
        acquired=true
        break
    fi
    sleep 0.2
done
if ! "$acquired"; then
    echo "GPU lock is busy: $lock (a stale lock must be inspected manually)" >&2
    exit 126
fi

pid=
cleanup() {
    if [[ -n $pid ]]; then
        kill -9 "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
    fi
    rmdir "$lock"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP

"$@" &
pid=$!
start=$SECONDS
while kill -0 "$pid" 2>/dev/null; do
    if (( SECONDS - start >= timeout )); then
        echo "GPU program exceeded ${timeout}s: $*" >&2
        exit 124
    fi
    rss=$(ps -o rss= -p "$pid" 2>/dev/null || true)
    if [[ $rss =~ ^[[:space:]]*([0-9]+)[[:space:]]*$ ]] &&
        (( BASH_REMATCH[1] > maxmem * 1024 )); then
        echo "GPU program exceeded ${maxmem} MB RSS: $*" >&2
        exit 125
    fi
    sleep 0.2
done
status=0
wait "$pid" || status=$?
pid=
exit "$status"
