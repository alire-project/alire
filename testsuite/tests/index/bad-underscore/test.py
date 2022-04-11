"""
Test underscores are properly rejected in property keys
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match(
             ".*TOML keys should use hyphens instead of underscores,"
             " but found key: depends_on.*",
             p.out)

print('SUCCESS')
