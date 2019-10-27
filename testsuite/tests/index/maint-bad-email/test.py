"""
Test that maintainers provide a plausible email
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match(
    'ERROR: Loading crate .*hello_world.toml: general: maintainers: '
    'Maintainers must have a valid email, but got: Mr. User\n'
    'ERROR: Cannot read valid property from maintainers\n'
    'ERROR: alr show unsuccessful\n',
    p.out)

print('SUCCESS')
