"""
Test that two crates providing the same third crate are incompatible
DISABLED because this is no longer conflicting. To be revisited when "forbids"
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# This test relies on two crates in the index:
# crate_equiv=2.0 also provides crate_virtual=1.0
# crate_clash=1.0 also provides crate_virtual=1.0
# Depending on the two of them cannot be solved, as that would mean two
# implementations of crate_virtual=1.0 at the same time

init_local_crate("xxx")
alr_with("crate_equiv")
alr_with("crate_clash")

match_solution("crate_equiv* (direct,missed)", escape=True)
# Because of alphabetical order, crate_clash is accepted first, and crate_equiv
# can no longer be accepted in the solution.

print('SUCCESS')
