"""
Test that two crates setting the same config var to the different values is not
ok.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import os
import platform

p = run_alr('get', '--build', 'hello_world', complain_on_error=False)
assert p.status != 0, "alr should have errored"

print(p.out)
assert_match(".*\n"
             "ERROR: Conflicting value for configuration variable"
             " 'libcrate_config_a.var_bool'"
             " from 'libcrate_config_b' \(true\)"
             " and 'libcrate_config_c' \(false\).*"
             ,
             p.out)

print('SUCCESS')
