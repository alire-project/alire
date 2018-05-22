#!/usr/bin/env bash

dst=`basename $IMAGE_NAME`
if [ "`find reltest -name '*.txt' | wc -l`" -gt 0 ]; then
    cp -fv reltest/*.txt status/$dst.txt
else
    echo "alr test failed to run in $dst" > status/$dst.txt
fi

git add status
git commit -m "alr test results for $dst [skip_ci]"
git push git@github.com:alire-project/alr.git $BRANCH
