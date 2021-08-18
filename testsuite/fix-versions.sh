#!/bin/bash

oldversion=1.0
newversion=1.1

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
