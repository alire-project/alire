"""
Detect a bad path in an environment variable
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Attempting to load the crate to show it should fail but only for the VAR3 bad variable

assert_match(".*VAR3: forbidden",
             run_alr("show", "crate", complain_on_error=False).out)

print('SUCCESS')
