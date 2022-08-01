"""
Test the usual use case in which a drop-in replacement crate forbids equivalent
crates in the solution. This works by both providing and forbidding the same
crate, which incidentally is the same modus operandi of apt-get "conflicts":
https://www.debian.org/doc/manuals/debian-reference/ch02.en.html#_package_dependencies
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# This test relies on two crates in the toolchain_index:
# crate_subst both provides and forbids crate_real
# crate_real is a regular crate only provided by itself and crate_subst

# The following has only one possible solution, which is for crate_subst
# providing both dependencies.
init_local_crate("test")
alr_with("crate_real")

# Check that this is initially solved with the regular crate. This is currently
# guaranteed by the solver attempting crates in alphabetical order. We will
# need eventually a way to disable equivalences (via pins, or solver config).
match_solution("crate_real=1.0.0 (origin: filesystem)", escape=True)

# Let's add the drop-in equivalent crate that provides+forbids crate_real
alr_with("crate_subst")
match_solution("crate_real=1.0.0 (crate_subst) (origin: filesystem)",
               escape=True)  # This is the substituted release
match_solution("crate_subst=1.0.0 (origin: filesystem)", escape=True)

print('SUCCESS')
