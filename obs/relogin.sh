#!/bin/bash

b=$(cat $HOME/data/obs/token.txt)

sudo ip netns exec protected sudo -u $USER -i curl -s --compressed -X POST -H "authorization: Bearer $b" -H "accept-encoding: gzip" -H "host: observer.tibia.com" -H "user-agent: Dart/3.5 (dart:io)" \
  "https://observer.tibia.com/api/v1.0/Account/Login" \
  | jq -r '.bearerToken' > $HOME/data/obs/new-token.txt

mv $HOME/data/obs/token.txt $HOME/data/obs/old-token.txt
mv $HOME/data/obs/new-token.txt $HOME/data/obs/token.txt
