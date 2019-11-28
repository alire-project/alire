"""
Test that long descriptions are rejected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match('ERROR: Loading crate .*hello_world.toml:'
             ' general: description: Description string is too long \(must be no more than [0-9]+\)\n'
             'ERROR: Cannot read valid property from description\n'
             'ERROR: alr show unsuccessful\n',
    p.out)

print('SUCCESS')
