"""
Test that the forbids field works for regular and provided crates
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# This test relies on three crates in the toolchain_index:
# crate_conflict=1.2.3 conflicts with crate_lone* and crate_virtual*
# crate_lone is a regular crate
# crate_virtual has no releases, but is provided by crate_conflict=1.2.3
# Crate conflict cannot appear with any of the others in a solution, because of
# its [forbids] table.

init_local_crate("conflict_lone")
alr_with("crate_conflict")
alr_with("crate_lone")
match_solution("crate_(conflict|lone)=.* \(origin:.*\)")   # has origin: solved
match_solution("crate_(conflict|lone)\* \(direct,missed:(conflict|skipped)\)")
# Because of load/solving details, we do not know which of the two crates is
# going to be missed/accepted in the solution, so we check there is one of each

init_local_crate("conflict_virtual")
alr_with("crate_conflict")
alr_with("crate_virtual")
match_solution("crate_(conflict|virtual)=.* \(origin:.*\)")
match_solution("crate_(conflict|virtual)\* \(direct,missed:(conflict|skipped)\)")

print('SUCCESS')
