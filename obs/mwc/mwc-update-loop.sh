#!/bin/bash

FORCE=false
while [[ $# -gt 0 ]]; do
    case "$1" in
        -f|--force)
            FORCE=true
            shift
            ;;
    esac
done

BASE_DIR="/home/tom/data/obs/mwc"
GET_UPDATE_SCRIPT="$BASE_DIR/get-mwc.sh"
OBS_RESPONSE="$BASE_DIR/obs-mwc.response"
LATEST_MWC="$BASE_DIR/latest-mwc.json"
LAST_MWC="$BASE_DIR/last-mwc.json"
MWC_HISTORY="$BASE_DIR/mwc-history"

export TZ="Europe/Berlin"

determine_ss_date() {
    hour=$(date --date="$1" "+%-H")
    if (( hour >= 10 )); then
        date -d "$1" "+%Y-%m-%d"
    else
        date -d "-1 day $1" "+%Y-%m-%d"
    fi
}

while true; do
    current_date_time=$(date +"%Y-%m-%dT%H:%M:%S")
    last_mwc_date_time=$(ls -1 $MWC_HISTORY | tail -1 | cut -d '.' -f 1)
    current_ss_date=$(determine_ss_date "$current_date_time")
    last_mwc_ss_date=$(determine_ss_date "$last_mwc_date_time")
    if [[ $FORCE = true || ( "$current_ss_date" > "$last_mwc_ss_date" ) ]]; then
        echo "[OBS] Trying to get update"
        # Get update from observer api
        $GET_UPDATE_SCRIPT > $OBS_RESPONSE

        # Create the file latest-mwc, handling downtime and errors gracefully
        jq -e 'any(.. | objects; has("notificationId"))' $OBS_RESPONSE >/dev/null 2>&1
        if [[ $? == 0 ]]; then
            jq '.' $OBS_RESPONSE > $LATEST_MWC
        fi

        # If latest-mwc differs from last-mwc, copy latest onto last and save the time
        if ! cmp -s $LATEST_MWC $LAST_MWC; then
            date -u
            date -u +"%Y-%m-%dT%H:%M:%SZ"
            cp $LATEST_MWC $LAST_MWC
            cp $LAST_MWC "$MWC_HISTORY/$(date -u +"%Y-%m-%dT%H:%M:%SZ").json"
        fi
    fi
    sleep 5
done
