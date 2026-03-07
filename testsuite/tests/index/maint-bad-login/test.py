"""
Test that maintainers-logins values can't be empty strings
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match(
    '.*Loading .*hello_world-0.1.0.toml:.*maintainers-logins:.*'
    'maintainers-logins values must be non-empty\n',
    p.out)

print('SUCCESS')
