#!/bin/bash

echo "[INFO] --------------------------------------------------------------"
echo "[INFO] start $0 $*"

echo "[INFO] os version"
uname -a
lsb_release -a

echo "[INFO] set HOME $1"
export HOME=$1

echo "[INFO] cd $HOME"
cd $HOME

echo "[INFO] run startup.sh"
echo "[INFO] --------------------------------------------------------------"

./startup.sh


