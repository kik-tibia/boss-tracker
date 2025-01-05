# Boss Tracker

Discord bot to post what bosses were recently killed, and predictions for what bosses are high chance to spawn

# Deployment steps

Build docker image  
1. `cd boss-tracker`
1. `sbt docker:publishLocal`

Copy to server  
1. `docker images`
1. `docker save boss-tracker:1.0.0 | gzip | ssh discord-bots docker load`

On the server
1. Create an env file for the env variables used in `src/main/scala/com/kiktibia/bosstracker/config/AppConfig.scala`
1. Create a folder in `$HOME/data/boss-tracker`, copy the files from the `data-example` folder in this repo, and also clone [tibia-kill-stats](https://github.com/tibiamaps/tibia-kill-stats) into that folder.
1. Run the docker container, pointing to the env file created in step 1 and making sure to select the correct version of the docker image: `docker run -d --env-file boss-tracker-dev.env -v $HOME/data/boss-tracker:$HOME/data/boss-tracker --user $(id -u $USER):$(id -g $USER) -e JAVA_OPTS="-Xms192m -Xmx680m" --name boss-tracker boss-tracker:1.0.0`
