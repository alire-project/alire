"""
Check toolchain selection assistant
"""

import subprocess
import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Activate the default compiler
p = run_alr("toolchain", "--select")

# Check that the newest native compiler is the Default now (vs Available)
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("2.0.0") + ".*Default.*",
             p.out)

# I've (mosteo) been unable to connect stdin with an alr launched via #
# subprocess.run, so no way to do further interactive tests at this time.
# My attempt follows. (I also attempted using subprocess.Popen.)

subprocess.run(["alr", "toolchain", "--select"],
               input="2\n", text=True,
               capture_output=True)
# This actually runs, but there is no input sent to alr. stdin=PIPE fails too.

print('SUCCESS')
