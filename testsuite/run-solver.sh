#!/bin/bash

set -o errexit

# Run subset of tests most likely to find a change in the solver's behavior

clear
python3 run.py -M1 solver/"$@"
python3 run.py -M1 with/"$@"
python3 run.py -M1 pin/"$@"
python3 run.py -M1 get/"$@"
