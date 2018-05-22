#!/usr/bin/env bash

dst=`basename $IMAGE_NAME`
if [ "`find reltest -name '*.txt' | wc -l`" -gt 0 ]; then
    cp -fv reltest/*.txt status/$dst
else
    echo "alr test failed in $dst" > status/$dst
fi

git add status
git commit -m "alr test results for $dst [skip_ci]"
git push origin $BRANCH
