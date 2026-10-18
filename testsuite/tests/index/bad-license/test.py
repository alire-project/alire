"""
Test that invalid licenses are rejected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('index', '--check',
            complain_on_error=False, debug=False, quiet=True)
assert_match('.*Loading .*hello_world-0.1.0.toml:'
             '.*licenses:.*Invalid license expression .* Missing closing parentheses.*\n',
             p.out)

print('SUCCESS')
