#!/bin/bash

docker run --rm -it \
    -p 8999:8999 \
    -p 8787:8787 \
    -v $PWD:/home/rstudio/app \
    -v $HOME/Downloads/:/home/benjamin.cretois/Downloads/ \
    -e DISABLE_AUTH=true \
    yellowstone_analysis:latest
