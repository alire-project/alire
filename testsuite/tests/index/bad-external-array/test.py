"""
Check improper 'external' key type
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('list', complain_on_error=False)
assert_match('.*Loading crate.*/index/he/hello_world.toml:.*'
             'external entries must be TOML arrays.*',
             p.out)


print('SUCCESS')
