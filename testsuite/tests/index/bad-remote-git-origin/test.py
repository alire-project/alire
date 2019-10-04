"""
Test that remote git origins are rejected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('list',
            complain_on_error=False)
assert_match(
    '.*Version Control System origins are only allowed for'
    ' repositories in the local filesystem.*',
    p.out)

print('SUCCESS')
