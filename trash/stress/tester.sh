#!/bin/bash

echo "use: $0 <times>"
echo Requires x0 and x1, alr projects in current folder
echo Times will be output to times-`date`.txt

now=`date -Is`

result=`pwd`/times-$now.txt
trace=`pwd`/trace-$now.txt

for i in `seq 1 $1`; do
    rm -rf ~/.cache/alire/sessions
    rm -rf ~/local/alr/bin/alr
    # Gnat seems to not detect changes below the second mark

    pushd x$(($i % 2)) 
    pwd
    ( /usr/bin/time -f %e alr dev -v --self ) 2>> "$result" | tee -a "$trace"
    popd
done

echo DONE
