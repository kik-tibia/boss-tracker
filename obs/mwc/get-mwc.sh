#!/bin/bash

b=$(cat $HOME/data/obs/token.txt)

sudo ip netns exec protected sudo -u $USER -i curl -s --compressed -H "authorization: Bearer $b" -H "accept-encoding: gzip" -H "host: observer.tibia.com" -H "user-agent: Dart/4.5 (dart:io)" \
  "https://observer.tibia.com/api/v1.0/MiniWorldChanges"
