"""
Test that origins of unknown kinds in indexes are properly reported.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world', complain_on_error=False, quiet=False)
assert_match(
    'ERROR: .* unknown origin: .*'
    '\nERROR: alr show unsuccessful'
    '\n', p.out)

print('SUCCESS')
