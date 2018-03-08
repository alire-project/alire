#!/bin/bash

# Launch latest GPL but with platform compiler
~/opt/gnat-gpl-2017/bin/gps -P `dirname $0`/../alr_env.gpr &
