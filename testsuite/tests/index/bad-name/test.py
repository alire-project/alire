"""
Check that specifying a malformed index name is properly reported
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('index', '--add', 'xx', '--name', 'xx',
            complain_on_error=False)
assert_match(
    '.*too short/long or contains illegal characters.*',
    p.out)

print('SUCCESS')
