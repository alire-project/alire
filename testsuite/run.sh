#!/bin/bash

[ -n "$TERM" ] && clear
python3 run.py --show-time-info -M1 "$@"
