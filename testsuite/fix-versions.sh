#!/bin/bash

oldversion=0.4
newversion=1.0

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
