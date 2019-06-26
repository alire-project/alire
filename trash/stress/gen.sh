#!/bin/bash

echo Use: $0 "<count> <src> <dst/>"

num=${1}
src=${2}
dst=${3}

[[ -f $src ]] || { echo "No src"; exit 1; }
[[ -d $dst ]] || { echo "No dst"; exit 1; }

for i in `seq -w 1 $num`; do
    file=$dst/alire-index-test$i.ads
    cp -fv $src $file
    sed -i "s/Template/Test$i/" $file
done
