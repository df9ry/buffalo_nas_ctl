#!/bin/sh
# stop_server.sh - Exakte Suche nach Prozessnamen

PID=$(ps -eo pid,comm | awk '$2 == "buffalo_nas_ctl" {print $1}')

if [ -n "$PID" ]; then
    echo "Stopping buffalo_nas_ctl (PID: $PID)..."
    kill -HUP "$PID"
    echo "Signal sent."
else
    echo "buffalo_nas_ctl is not running."
fi
