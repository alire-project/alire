"""
Test invalid action command
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match('ERROR: Loading .* actions command must'
             ' be an array of string\(s\)\n',
             p.out)

print('SUCCESS')
