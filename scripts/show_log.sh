#!/bin/bash
# Buffalo NAS CTL Log Viewer

case "$1" in
    "")
        # Normale Logs anzeigen
        sudo journalctl -u buffalo-nas-ctl.service -n 50
        ;;
    "f"|"follow")
        # Logs in Echtzeit verfolgen
        sudo journalctl -u buffalo-nas-ctl.service -f
        ;;
    "e"|"errors")
        # Nur Fehler anzeigen
        sudo journalctl -u buffalo-nas-ctl.service -p err -n 50
        ;;
    "t"|"today")
        # Heutige Logs
        sudo journalctl -u buffalo-nas-ctl.service --since=today
        ;;
    "h"|"help")
        echo "Usage: buffalo-log [option]"
        echo "  (keine)  - Letzte 50 Logeinträge"
        echo "  f        - Logs in Echtzeit folgen"
        echo "  e        - Nur Fehler anzeigen"
        echo "  t        - Heutige Logs"
        ;;
    *)
        # Eigener Parameter
        sudo journalctl -u buffalo-nas-ctl.service "$@"
        ;;
esac
