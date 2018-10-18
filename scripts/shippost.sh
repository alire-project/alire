#!/usr/bin/env bash

exit 0
# TEMPORARILY DISABLED

dst=`basename $IMAGE_NAME`
if [ "`find reltest -name '*.md' | wc -l`" -gt 0 ]; then
    cp -fv reltest/*.md status/$dst.md
else
    echo "alr test failed to run in $dst" > status/$dst.md
fi

git pull
git add status
git commit -m "alr test results for $dst [skip ci]"
git push git@github.com:alire-project/alr.git $BRANCH
