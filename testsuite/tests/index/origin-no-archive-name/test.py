"""
Test that origins lacking archive names in indexes are properly reported.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world', complain_on_error=False, quiet=False)
assert_match(
    'ERROR:.*missing mandatory archive-name'
    '\nERROR: alr show unsuccessful'
    '\n', p.out)

print('SUCCESS')
