"""
Test solver using the "provides" field for regular crates
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# This test relies on two crates in the index: crate_lone=1.0 is unavailable.
# crate_equiv=2.0 also provides crate_lone=1.0 and crate_virtual=1.0.
# Finally there is crate_lone=2.0 that is available and nobody else provides.

init_local_crate("xxx")
alr_with("crate_lone^1")

# Since crate_lone is unavailable, in the solution we should find crate_equiv:
match_solution("crate_lone=2.0.0 (crate_equiv)", escape=True)

# Likewise, a dependency on crate_virtual will be fulfilled by the same crate
alr_with("crate_virtual")
match_solution("crate_virtual=2.0.0 (crate_equiv)", escape=True)

# Whereas a dependency on crate_equiv will show plainly without equivalence
alr_with("crate_equiv")
match_solution(
    "Dependencies (solution):\n"
    "   crate_equiv=2.0.0 (origin: filesystem)\n"
    "   crate_lone=2.0.0 (crate_equiv) (origin: filesystem)\n"
    "   crate_virtual=2.0.0 (crate_equiv) (origin: filesystem)\n",
    escape=True)

# Finally check that a dependency on crate_lone^2 is only fulfilled by itself
os.chdir("..")
init_local_crate("yyy")
alr_with("crate_lone^2")
match_solution("crate_lone=2.0.0 (origin: filesystem)", escape=True)

print('SUCCESS')
