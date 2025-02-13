#!/bin/bash

set -o errexit

# Run subset of tests most likely to find a change in the solver's behavior

opts=--show-time-info -M1

clear
python3 run.py $opts solver/"$@"
python3 run.py $opts with/"$@"
python3 run.py $opts pin/"$@"
python3 run.py $opts get/"$@"
