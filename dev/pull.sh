#/bin/bash

git pull --all
git submodule update --init --recursive
git-recurse "git pull --ff-only"

echo ' '
git submodule status --recursive
