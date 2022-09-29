"""
Check that 'alr -h <command>' produces same output as 'alr help command'
"""

import re

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

####################################
# First check, with valid help topic
p1 = run_alr('-h', 'get',   quiet=False)
p2 = run_alr('help', 'get', quiet=False)

# Verify we got the expected help
assert_match("SUMMARY\n   Fetch a published crate.*", p1.out, flags=re.S)

# Verify equality
assert p1.out == p2.out, "Mismatch in outputs: {} != {}".format(p1.out, p2.out)

#######################################
# Second check, with invalid help topic

p1 = run_alr('-h', 'non_existing_command',   complain_on_error=False, quiet=False)
p2 = run_alr('help', 'non_existing_command', complain_on_error=False, quiet=False)

# Verify we got the expected error message
assert_match(".*o help found for:.*", p1.out)

# Verify equality
assert p1.out == p2.out, "Mismatch in outputs: {} != {}".format(p1.out, p2.out)

print('SUCCESS')
