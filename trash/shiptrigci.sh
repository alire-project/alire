#!/usr/bin/env bash

trap 'echo "ERROR at line ${LINENO} (code: $?)" >&2' ERR 
trap 'echo "Interrupted" >&2 ; exit 1' INT 

set -o errexit
set -o nounset

function crate_test() {
    git clone https://github.com/alire-project/alire-crates-ci
    pushd alire-crates-ci
    date -u > last-run
    git add last-run
    git commit -m "Crate check triggered from alire"
    git push
    popd
}

# Check crates if commiting to master or stable
if [ "$IS_PULL_REQUEST" == false ]; then
    if [ "$BRANCH" == "master" ] || [ "$BRANCH" == "stable" ]; then
        echo Triggering downstream crate CI test...
    fi
fi

# do it always until tested working
crate_test
