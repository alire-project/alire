#!/bin/bash

set -o errexit
set -o nounset

case $OSTYPE in
    linux-gnu)
        os=linux;;
    *)
        echo Unsupported platform: "[$OSTYPE]"
        exit 1;;
esac
