#!/bin/bash
set -o pipefail

API_URL="http://borg.tega.internal:8000/api/poll"
POLL_INTERVAL=30
PARENT_PID=$PPID

# Prüft, ob der übergeordnete Prozess noch existiert und kein Zombie ist.
check_parent() {
    # Wenn Parent PID 1 ist, wurde unser Prozess adoptiert -> beenden
    if [ "$PARENT_PID" -eq 1 ]; then
        echo "Parent process died (adopted by init). Exiting." >&2
        return 1
    fi

    # Existiert der Parent-Prozess?
    if ! kill -0 "$PARENT_PID" 2>/dev/null; then
        echo "Parent process $PARENT_PID no longer exists. Exiting." >&2
        return 1
    fi

    # Zombie-Check
    if [ -e "/proc/$PARENT_PID/status" ] && grep -q '^State:.*Z' "/proc/$PARENT_PID/status"; then
        echo "Parent process $PARENT_PID is a zombie. Exiting." >&2
        return 1
    fi

    return 0
}

# Führt einen einzelnen Poll durch. Gibt 0 zurück bei HTTP 200, sonst 1.
do_poll() {
    local http_status
    http_status=$(curl -s -o /dev/null -w "%{http_code}" --max-time 10 "$API_URL" 2>/dev/null)

    case "$http_status" in
        200)
            return 0
            ;;
        *)
            # Jeder andere Status (auch 201, 404, 500, Timeout, etc.) ist ein Fehler
            echo "ERROR: Unexpected HTTP status $http_status from API." >&2
            return 1
            ;;
    esac
}

# Hauptschleife
while true; do
    if ! do_poll; then
        echo "Continue ..."
        #exit 1
    fi

    if check_parent; then
        sleep "$POLL_INTERVAL"
    else
        exit 1
    fi
done
