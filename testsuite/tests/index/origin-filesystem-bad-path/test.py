"""
Test that invalid filesystem origins are reported.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('list', complain_on_error=False, quiet=True)

# Since the location reported is an absolute path, and thus filesystem
# dependent, check only that the beginning of the error is there:
assert_match(
    'ERROR: Local origin path is not a valid directory: .*non-existing-path'
    '\nERROR: alr list unsuccessful\n$',
    p.out)

print('SUCCESS')
