#!/bin/bash

PRJ_DIR=/root/work/azhaskell-func-handler/custom-handler-if

docker run --rm  \
  -v $PWD:/conf \
  -v $PRJ_DIR:/project \
  openapitools/openapi-generator-cli generate -i /conf/openapi.yaml -g haskell -o /project


