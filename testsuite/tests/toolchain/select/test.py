"""
Check toolchain selection assistant
"""

import os
import re
import subprocess

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match

p = run_alr("index")
print(p.out)

# Validation duplicated release arguments
p = run_alr("toolchain", "--select", "gnat_native=1.2.3", "gnat_native=4.5.6",
            complain_on_error=False)
assert_match(".*Release arguments contain duplicated crates", p.out)

# Activate the default compiler
p = run_alr("toolchain", "--select")

# Check that the newest native compiler is the Default now (vs Available)
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("8888.0.0") + ".*Default.*",
             p.out)

# Select an older compiler as default
run_alr("toolchain", "--select", "gnat_native=7777")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Default.*",
             p.out)

# Test local selection by configuring locally inside a crate
init_local_crate()
run_alr("toolchain", "--select", "gnat_native=8888", "--local")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("8888.0.0") + ".*Default.*",
             p.out)
# And check that outside the global selection is still in effect
os.chdir("..")
p = run_alr("toolchain")
assert_match(".*gnat_native.*" + re.escape("7777.0.0") + ".*Default.*",
             p.out)

# I've (mosteo) been unable to connect stdin with an alr launched via #
# subprocess.run, so no way to do further interactive tests at this time.
# My attempt follows. (I also attempted using subprocess.Popen.)

subprocess.run(["alr", "toolchain", "--select"],
               input="2\n", text=True,
               capture_output=True)
# This actually runs, but there is no input sent to alr. stdin=PIPE fails too.

print('SUCCESS')
