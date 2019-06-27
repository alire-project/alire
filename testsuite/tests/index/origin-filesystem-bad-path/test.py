"""
Test that filesystem origins with a bad path fail on get
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq

import re

p = run_alr('list', complain_on_error=False, quiet=False)

# Since the location reported is filesystem dependent, check only that the beginning
# of the error is there:

assert re.match('ERROR: Local origin path is not a valid directory: .*libhello_1.0.0_xxxxxxxx\n', p.out), \
       "Output does not contain expected error:\n" + p.out

print('SUCCESS')
