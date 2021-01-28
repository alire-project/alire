"""
Test that no alien fields appear in index metadata
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr("crates", complain_on_error=False)
assert_match(
    '.*index metadata contains unexpected fields.*',
    p.out)

print('SUCCESS')
