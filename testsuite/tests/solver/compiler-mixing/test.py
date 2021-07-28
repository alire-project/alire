"""
Check mixing gnat/gnat_xxx dependencies without configured preferred compiler
"""

import subprocess
import os

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# Prepare a couple of dependencies, one depending on gnat, and another one
# depending on gnat_native.

init_local_crate("dep_generic")
alr_with("gnat")
os.chdir("..")

init_local_crate("dep_targeted")
alr_with("gnat_native")  # This step also installs the native compiler
os.chdir("..")

# First we check that a root generic dependency mixes well with either of the
# two dependencies

init_local_crate("xxx_generic_generic")
run_alr("with", "--use=../dep_generic")
alr_with("gnat")

# gnat x gnat results in the native available compiler being used, preferred
# over the external also available compiler
match_solution("gnat=2.0.0 (gnat_native) (installed) (origin: binary_archive)",
               escape=True)

# If we add a precise dependency on e.g. the external compiler, this should
# override the native compiler
alr_with("gnat_external")
match_solution("gnat=.*" + e("(gnat_external) (installed)") + ".*" +
               "gnat_external=.*" + e("(installed) (origin: external)") + ".*")

# Let us swap the generic dependency with a targeted dependency, starting from
# scratch

os.chdir("..")
init_local_crate("xxx_generic_targeted")
run_alr("with", "--use=../dep_targeted")
alr_with("gnat")

# In this case the only possible solution is with the targeted compiler
match_solution("gnat=" + e("2.0.0 (gnat_native) (installed)") + ".*" +
               "gnat_native=" + e("2.0.0 (installed)") + ".*")

# Second, we check a root targeted gnat with both dependencies

os.chdir("..")
init_local_crate("xxx_targeted_generic")
run_alr("with", "--use=../dep_generic")
alr_with("gnat_native")

# In this case the only possible solution is with the targeted compiler. The
# Generic dependency also appears, coming from the dep_generic crate, and
# because there is no default compiler selected
match_solution("gnat=" + e("2.0.0 (gnat_native) (installed)") + ".*" +
               "gnat_native=" + e("2.0.0 (installed)") + ".*")

# Last combination is targeted x targeted
os.chdir("..")
init_local_crate("xxx_targeted_targeted")
run_alr("with", "--use=../dep_targeted")
alr_with("gnat_native")

# In this case the only possible solution is with the targeted compiler. The
# generic dependency no longer exists, as nobody requested a generic gnat.
match_solution("gnat_native=" + e("2.0.0 (installed)") + ".*")
p = run_alr("with", "--solve")
assert "gnat=" not in p.out, "Unexpected output"

print('SUCCESS')
