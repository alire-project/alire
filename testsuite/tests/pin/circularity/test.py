"""
Verify pin circularity detection
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import on_windows

import os

# Obvious self-pinning detection
init_local_crate()
alr_pin("xxx", path=".", update=False)
p = run_alr("pin", complain_on_error=False)
assert_match(".*"
             "ERROR: Pin circularity detected when adding pin xxx --> xxx:\n"
             "ERROR:    Last manifest in the cycle is .*\n",
             p.out)

print('SUCCESS')
