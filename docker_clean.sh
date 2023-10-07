#!/bin/sh

docker network prune -f
docker volume prune -f
docker container prune -f
docker image prune -a -f

docker system prune --volumes -f
