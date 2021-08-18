"""
Check solving with a configured preferred compiler
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# Select the default preferred compiler, which is the native packaged one
run_alr("toolchain", "--select")

# Init a crate depending on gnat

init_local_crate("xxx")
alr_with("gnat*")

# Will appear in the solution as generic fulfilled by the preferred compiler
match_solution("gnat=2.0.0 (gnat_native) (installed)", escape=True)

# Selecting another default will cause a corresponding change in the solution
run_alr("config", "--set", "toolchain.use.gnat", "gnat_cross_2=1")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2) (installed)", escape=True)

# Adding another incompatible compiler dependency should result in overriding
# the configured one
alr_with("gnat_cross_1")

# Both dependencies will appear in the solution, matching the same crate
match_solution("gnat=9999.0.0 \(gnat_cross_1\) \(installed\).*"
               "gnat_cross_1=9999.0.0 \(installed\)")

print('SUCCESS')
