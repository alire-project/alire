#!/bin/bash

oldversion=0.4
newversion=0.5

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
