#!/usr/bin/env bash

cd "$(dirname "$0")"

DATA_DIR="tibia-kill-stats/data/nefera"
OUTPUT_DIR="boss-kill-history"
BOSS="White Pale"

jq -c --arg boss "$BOSS" '[.killstatistics.entries[] | { (.race): .last_day_killed }] | add | .[$boss] // 0 | { (input_filename | gsub(".*/|\\.json$";"")): .}' $DATA_DIR/*.json | grep -v '"latest"' > "$OUTPUT_DIR/$BOSS.json"
