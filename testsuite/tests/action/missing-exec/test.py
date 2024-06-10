"""
Check that an action trying to run a missing executable reports such an error.
"""

from drivers.alr import run_alr, add_action, init_local_crate
from drivers.asserts import assert_substring

init_local_crate()

add_action('pre-build', ['fake-missing-exec'])

p = run_alr("build", complain_on_error=False)

assert_substring("Command not found  [fake-missing-exec]",
                 p.out)

print('SUCCESS')
