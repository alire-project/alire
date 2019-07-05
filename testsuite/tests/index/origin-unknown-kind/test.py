"""
Test that origins of unknown kinds in indexes are properly reported.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


p = run_alr('show', 'hello_world', complain_on_error=False, quiet=False)
assert_eq(
    'ERROR: hello_world (origin): Unable to determine archive format from file'
    ' extension'
    '\nERROR: alr show unsuccessful'
    '\n', p.out)

print('SUCCESS')
