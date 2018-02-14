#/bin/bash

branch=${1:-devel}

git pull
git submodule update --recursive
git-recurse "git checkout $branch"
git-recurse "git pull --ff-only"

echo ' '
git submodule status --recursive
