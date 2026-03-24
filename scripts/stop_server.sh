#!/bin/sh
# stop_server.sh - Exakte Suche nach Prozessnamen

for PID in $(ps -eo pid,comm | awk '$2 == "buffalo_nas_ctl" {print $1}')
do
    echo "Stopping PID: $PID ..."
    echo "  Send HUP"
    kill -HUP $PID
    sleep 3
    if [ -d "/proc/$PID" ]; then
        echo "  Send KILL"
        kill -9 $PID
        sleep 3
        if [ -d "/proc/$PID" ]; then
            echo "  Unable to kill $PID"
            exit 1
        fi
    fi
    echo "  OK, process exited"
done
