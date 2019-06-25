"""
Test the behavior of "alr get" to get a crate that does not exist.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


p = run_alr('get', 'does_not_exist', complain_on_error=False, quiet=False)
assert_eq(1, p.status)
assert_eq('Project [does_not_exist] does not exist in the catalog.'
          '\nERROR: alr get unsuccessful',
          p.out.strip())
assert_eq([], glob('does_not_exist*'))

print('SUCCESS')
