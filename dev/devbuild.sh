#!/bin/bash

case $OSTYPE in
   linux*)  export ALIRE_OS=linux;;
   msys)    export ALIRE_OS=windows;;
   darwin*) export ALIRE_OS=macos;;
   *)       echo Unsupported host OS: OSTYPE=$OSTYPE; exit 1;;
esac

gprbuild -j0 -r -p -P `dirname $0`/../alr_env.gpr
