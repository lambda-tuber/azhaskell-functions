#!/bin/sh

docker run --rm -it \
  -p80:80 -p2222:2222 \
  --env-file local.settings.env \
  --name azhaskell-func-con \
  azhaskell-func-image

