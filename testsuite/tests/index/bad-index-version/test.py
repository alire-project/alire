"""
Test that mismatched index version is detected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr("search", "--crates", complain_on_error=False)
assert_match(
    '.*index version \(0\.0\.0\) is older than that expected by alr \(.*\).*',
    p.out)

print('SUCCESS')
