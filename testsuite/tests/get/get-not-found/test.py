"""
Test the behavior of "alr get" to get a crate that does not exist.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match


p = run_alr('get', 'does_not_exist', complain_on_error=False)
assert_eq(1, p.status)
assert_match('.*Crate \[does_not_exist\] does not exist in the index\.\n',
             p.out)
assert_eq([], glob('does_not_exist*'))

print('SUCCESS')
