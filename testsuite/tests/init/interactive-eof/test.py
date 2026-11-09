"""
Closing the input at a prompt (Ctrl-D) must be a clean interruption by the user
"""

import os

from drivers.alr import run_alr_interactive
from drivers.asserts import assert_not_substring

p = run_alr_interactive(['init', '--bin', 'xxx'],
                        output=['> '],
                        input=[b'\x04'],
                        timeout=5,
                        complain_on_error=False)

assert_not_substring("raised", p)
assert_not_substring("Unexpected", p)
assert_not_substring("report", p)
assert not os.path.exists(os.path.join("xxx", "alire.toml")), p

print('SUCCESS')
