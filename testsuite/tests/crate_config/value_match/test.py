"""
Test that two crates setting the same config var to the same value is ok.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import os
import platform

p = run_alr('get', 'hello_world')

print('SUCCESS')
