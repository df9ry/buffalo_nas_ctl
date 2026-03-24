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
    echo "Marker-File $MARKER_FILE found, already mounted."
    exit 0
fi

echo "Attempt to mount NAS (max. $MAX_ATTEMPTS tries all $SLEEP_SECONDS sec.)"

while (( attempt < MAX_ATTEMPTS )); do
    ((attempt++))

    echo "Attempt $attempt/$MAX_ATTEMPTS ..."

    # 1. Vorherigen (eventuell hängenden) Mount lösen
    if mountpoint -q "$MOUNT_POINT" 2>/dev/null; then
        echo "  Clear up dangling mounts ..."
        umount -f -l "$MOUNT_POINT" 2>/dev/null || true
        sleep 1
    fi

    # 2. Mount ausführen (Parameter kommen aus /etc/fstab)
    echo "  Try to mount $MOUNT_POINT ..."
    if mount "$MOUNT_POINT" >/dev/null 2>&1; then
        # kurze Wartezeit → Dateisystem stabilisieren
        sleep 1.5

        if [[ -f "$MARKER_FILE" ]]; then
            echo "Found $MARKER_FILE:"
            echo "Mount NAS was successful after $attempt attempts."
            exit 0
        else
            echo "  Warnung: Good mount, but $MARKER_FILE is missing!"
        fi
    else
        echo "  Mount failed with error code $?"
    fi

    # Nicht erfolgreich → nächste Runde
    if (( attempt < MAX_ATTEMPTS )); then
        echo "  Pause for $SLEEP_SECONDS seconds ..."
        sleep "$SLEEP_SECONDS"
    fi
done

echo "ERROR: Unable to mount NAS after $MAX_ATTEMPTS attempts." >&2
echo "Marker-File $MARKER_FILE not found." >&2
exit 1
