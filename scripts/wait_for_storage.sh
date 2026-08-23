#!/bin/bash
set -euo pipefail

API_URL="http://borg.tega.internal:8000/api/poll"
MAX_ATTEMPTS=18
SLEEP_SECONDS=10

for attempt in $(seq 1 $MAX_ATTEMPTS); do
    echo "Polling storage status (Attempt $attempt of $MAX_ATTEMPTS)..."

    # Führe den curl aus, speichere HTTP-Statuscode und Body
    HTTP_STATUS=$(curl -s -o /tmp/poll_response.json -w "%{http_code}" "$API_URL" 2>/dev/null || echo "000")

    case "$HTTP_STATUS" in
        200)
            echo "Storage is ready."
            exit 0
            ;;
        201)
            echo "Storage is starting, waiting ${SLEEP_SECONDS} seconds..."
            sleep "$SLEEP_SECONDS"
            ;;
        *)
            echo "ERROR: Unexpected HTTP status $HTTP_STATUS from API. Aborting."
            echo "Response body (if any):"
            cat /tmp/poll_response.json 2>/dev/null || echo "<no response>"
            exit 1
            ;;
    esac
done

echo "ERROR: Storage did not become ready after $MAX_ATTEMPTS attempts. Aborting."
exit 1
