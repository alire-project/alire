"""
Test that maintainers provide a plausible GitHub login
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match(
    'ERROR:.*Loading .*hello_world-0.1.0.toml: maintainers-logins: '
    'maintainers-logins must be a valid GitHub login, but got: mr.user\n',
    p.out)

print('SUCCESS')
