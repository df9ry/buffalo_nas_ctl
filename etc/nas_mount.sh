#!/usr/bin/env bash
# nas_mount.sh
# Prüft, ob die NAS-Webseite erreichbar ist, und mountet dann den Share.
# Verlässt sich nur auf das Mount selbst (kein Marker-File mehr).

# ────────────────────────────────────────────────
# Konfiguration
# ────────────────────────────────────────────────

MOUNT_POINT="/mnt/nas"
NAS_URL="http://192.168.178.153"

# Webseiten-Check
WEB_MAX_ATTEMPTS=18
WEB_SLEEP_SECONDS=20

# Mount-Check
MOUNT_MAX_ATTEMPTS=3
MOUNT_SLEEP_SECONDS=10

# ────────────────────────────────────────────────

set -u

# 2. Webseite der NAS prüfen
echo "Checking NAS web interface (interval ${WEB_SLEEP_SECONDS}s)..."
attempt=0
while (( attempt < WEB_MAX_ATTEMPTS )); do
    ((attempt++))
    echo "Web check attempt $attempt/$WEB_MAX_ATTEMPTS ..."

    http_code=$(curl -s -o /dev/null -w "%{http_code}" --max-time 15 --location "$NAS_URL" 2>/dev/null)

    if [[ "$http_code" == "200" ]]; then
        echo "NAS web interface responded with HTTP 200."
        break
    else
        echo "  Got HTTP $http_code, not ready yet."
        if (( attempt < WEB_MAX_ATTEMPTS )); then
            echo "  Waiting ${WEB_SLEEP_SECONDS} seconds ..."
            sleep "$WEB_SLEEP_SECONDS"
        fi
    fi
done

if [[ "$http_code" != "200" ]]; then
    echo "ERROR: NAS web interface not reachable after $WEB_MAX_ATTEMPTS attempts." >&2
    exit 1
fi

sleep 3

# Prüfen, ob bereits korrekt gemountet
if mountpoint -q "$MOUNT_POINT" && grep -q " //192.168.178.153/TEGA " /proc/mounts; then
    echo "NAS share is already mounted and ready."
    exit 0
fi

# 3. Mount-Versuche
echo "Attempting to mount NAS share (max. $MOUNT_MAX_ATTEMPTS tries)..."
for ((attempt=1; attempt<=MOUNT_MAX_ATTEMPTS; attempt++)); do
    echo "Mount attempt $attempt/$MOUNT_MAX_ATTEMPTS ..."

    umount -l $MOUNT_POINT 2>/dev/null
    if mount "$MOUNT_POINT"
    then

        sleep 3
        echo "Mount successful after $attempt attempt(s)."
        exit 0
    else
        echo "  Mount failed with error code $?"
        if (( attempt < MOUNT_MAX_ATTEMPTS )); then
            echo "  Waiting ${MOUNT_SLEEP_SECONDS} seconds before next try..."
            sleep "$MOUNT_SLEEP_SECONDS"
        fi
    fi
done

echo "ERROR: Mount failed after $MOUNT_MAX_ATTEMPTS attempts." >&2
exit 1
