# -*- coding: utf-8 -*-
# Just to be sure

"""
Verify that by default, alr prints the proper UTF-8 output
"""

import os
import pty
import subprocess

from drivers.alr import init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import on_windows
from e3.testsuite.driver.classic import TestSkip

# alr on Windows doesn't produce rich output at the moment, so we skip
if on_windows():
    raise TestSkip("Platform does not produce UTF-8 output")

# Check the info char in a successful build. If that is proper, others should be
# too.

init_local_crate()

# We run it directly to pass the tty detection

# Create a new pseudo-terminal pair
parent, child = pty.openpty()
proc = subprocess.Popen(["alr", "build"],
                        stdin=child, stdout=child, stderr=child)

# Close the child side in the parent process, as it is not needed
os.close(child)

# Wait for it to complete
proc.wait()

# Read output
output = os.read(parent, 1024) # Should be far less that 1024 bytes of output

os.close(parent)

# The ANSI color codes are also there between "ⓘ" and "Building", alr can't
# produce extended output but no color (maybe it should?)
assert_match(".*ⓘ.*Building.*",
             output.decode("utf-8"))

print('SUCCESS')
