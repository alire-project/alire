"""
Test that maintainers provide a plausible GitHub login
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match(
    'ERROR: Loading crate .*hello_world.toml: general: maintainers-logins: '
    'maintainers-logins must be a valid GitHub login, but got: mr.user\n'
    'ERROR: Cannot read valid property from maintainers-logins\n',
    p.out)

print('SUCCESS')
