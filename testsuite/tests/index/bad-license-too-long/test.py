"""
Test that long licenses are rejected
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


p = run_alr('show', 'hello_world',
            complain_on_error=False, debug=False, quiet=True)
assert_match('.*Loading .*hello_world-0.1.0.toml:'
             '.*licenses:.*License expression too long \(must be no more than [0-9]* chars\)\n',
             p.out)

print('SUCCESS')
