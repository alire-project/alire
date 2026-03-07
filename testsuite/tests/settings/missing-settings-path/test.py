"""
Verify that errors are properly handled when no settings path is given
"""

import os
from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

switch = "--settings"
short = "-s"

p = run_alr(switch, complain_on_error=False)
assert_match(f"ERROR: Switch {switch} requires argument.*", p.out, flags=re.S)

p = run_alr(short, complain_on_error=False)
assert_match(f"ERROR: Switch {short} requires argument.*", p.out, flags=re.S)

# Check also failure in case of duplication of switch
path = os.getcwd()
p = run_alr(f"{short}", path, f"{switch}={path}", "version",
            complain_on_error=False)
assert_match(".*Only one of .* allowed",
             p.out)

print('SUCCESS')
