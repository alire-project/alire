#!/bin/bash

oldversion=0.5
newversion=1.0

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
