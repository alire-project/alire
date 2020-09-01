#!/bin/bash

oldversion=0.2
newversion=0.4

find . -type f -name index.toml -exec sed -i "s/$oldversion/$newversion/" {} \;
