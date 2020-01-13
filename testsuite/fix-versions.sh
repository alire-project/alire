#!/bin/bash

oldversion=0.1
newversion=0.2

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
