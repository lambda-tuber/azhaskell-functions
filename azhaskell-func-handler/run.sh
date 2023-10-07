#!/bin/bash

docker run --name haskell-dev-con --mount type=bind,src=/root/work/azhaskell-func-handler,dst=/haskell --rm  haskell-dev-image /haskell

cp -p custom-handler/custom-handler ../azhaskell-func-con/wwwroot/

