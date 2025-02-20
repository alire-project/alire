#!/bin/bash

clear
python3 run.py -j$(nproc) --show-time-info -M1 "$@"
