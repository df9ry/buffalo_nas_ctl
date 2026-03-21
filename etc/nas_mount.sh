#!/usr/bin/env bash
# nas_mount.sh
# Mountet einen per /etc/fstab definierten NAS-Mountpunkt mit Retry-Logik
# und prüft Erfolg über eine Marker-Datei

# ────────────────────────────────────────────────
# Konfiguration – nur diese Werte anpassen
# ────────────────────────────────────────────────

MOUNT_POINT="/mnt/nas"
MARKER_FILE="${MOUNT_POINT}/.mark"

MAX_ATTEMPTS=18
SLEEP_SECONDS=10

# ────────────────────────────────────────────────

set -u

attempt=0

if [[ -f "$MARKER_FILE" ]]; then
    echo "Marker-Datei $MARKER_FILE gefunden → NAS scheint bereits gemountet."
    exit 0
fi

echo "Versuche NAS zu mounten (max. $MAX_ATTEMPTS Versuche, je $SLEEP_SECONDS s Pause)"

while (( attempt < MAX_ATTEMPTS )); do
    ((attempt++))

    echo "Versuch $attempt/$MAX_ATTEMPTS ..."

    # 1. Vorherigen (eventuell hängenden) Mount lösen
    if mountpoint -q "$MOUNT_POINT" 2>/dev/null; then
        echo "  → löse vorherigen/hängenden Mount ..."
        umount -f -l "$MOUNT_POINT" 2>/dev/null || true
        sleep 1
    fi

    # 2. Mount ausführen (Parameter kommen aus /etc/fstab)
    echo "  → führe mount $MOUNT_POINT aus ..."
    if mount "$MOUNT_POINT" >/dev/null 2>&1; then
        # kurze Wartezeit → Dateisystem stabilisieren
        sleep 1.5

        if [[ -f "$MARKER_FILE" ]]; then
            echo "Erfolg: Marker-Datei $MARKER_FILE gefunden."
            echo "NAS erfolgreich gemountet nach $attempt Versuchen."
            exit 0
        else
            echo "  Warnung: mount meldet Erfolg, aber Marker $MARKER_FILE fehlt!"
        fi
    else
        echo "  mount fehlgeschlagen (rc=$?)"
    fi

    # Nicht erfolgreich → nächste Runde
    if (( attempt < MAX_ATTEMPTS )); then
        echo "  → warte $SLEEP_SECONDS Sekunden ..."
        sleep "$SLEEP_SECONDS"
    fi
done

echo "FEHLER: NAS konnte nach $MAX_ATTEMPTS Versuchen nicht erfolgreich gemountet werden." >&2
echo "Marker-Datei $MARKER_FILE wurde nicht gefunden." >&2
exit 1
