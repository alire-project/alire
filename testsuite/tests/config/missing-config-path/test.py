"""
Verify that errors are properly handled when no config path is given
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr("--config", complain_on_error=False)
assert p.status != 0, "command should fail"
assert_match("ERROR: Switch --config requires argument.*", p.out, flags=re.S)

p = run_alr("-c", complain_on_error=False)
assert p.status != 0, "command should fail"
assert_match("ERROR: Switch -c requires argument.*", p.out, flags=re.S)

print('SUCCESS')
