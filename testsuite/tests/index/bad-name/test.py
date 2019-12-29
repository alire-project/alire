"""
Check that specifying a malformed index name is properly reported
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('index', '--add', 'xx', '--name', 'xx',
            complain_on_error=False, debug=False)
assert_match('.*Identifier too short.*', p.out)

print('SUCCESS')
