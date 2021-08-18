"""
Test that two crates providing the same third crate are compatible
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# This test relies on two crates in the index:
# crate_virt_1=2.0 also provides crate_virtual=1.0
# crate_virt_2=1.0 also provides crate_virtual=1.0

# Verify that these crates provide the same virtual release
p = run_alr("show", "crate_virt_1")
assert_match(".*Provides: crate_virtual=1.0.0.*", p.out)
p = run_alr("show", "crate_virt_2")
assert_match(".*Provides: crate_virtual=1.0.0.*", p.out)

init_local_crate("xxx")
alr_with("crate_virt_1")
alr_with("crate_virt_2")

# Both crates must appear in the solution
match_solution("crate_virt_1=2.0.0 (origin: filesystem)", escape=True)
match_solution("crate_virt_2=1.0.0 (origin: filesystem)", escape=True)

print('SUCCESS')
