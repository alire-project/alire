"""
Check mixing gnat/gnat_xxx dependencies without configured preferred compiler
"""

import subprocess
import os
import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from re import escape as e

# Verify only external compiler available
p = run_alr("toolchain")
assert_match(".*\n"  # Headers
             "gnat_external.*Available.*Detected.*\n",
             p.out)

# Capture version
version = re.search("gnat_external ([0-9.]+)", p.out, re.MULTILINE).group(1)

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

# gnat x gnat results in the external available compiler being used, preferred
# over the native also available compiler (but not selected)
match_solution(f"gnat={version} (gnat_external)",
               escape=True)

# If we add a precise dependency on e.g. the installed native compiler, this
# should override the external compiler
alr_with("gnat_native")
match_solution("gnat=8888.0.0 (gnat_native)", escape=True)
match_solution("gnat_native=8888.0.0", escape=True)

# Let us swap the generic dependency with a targeted dependency, starting from
# scratch

os.chdir("..")
init_local_crate("xxx_generic_targeted")
run_alr("with", "--use=../dep_targeted")
alr_with("gnat")

# In this case the only possible solution is with the targeted compiler
match_solution("gnat=" + e("8888.0.0 (gnat_native)") + ".*" +
               "gnat_native=" + e("8888.0.0") + ".*")

# Second, we check a root targeted gnat with both dependencies

os.chdir("..")
init_local_crate("xxx_targeted_generic")
run_alr("with", "--use=../dep_generic")
alr_with("gnat_native")

# In this case the only possible solution is with the targeted compiler. The
# Generic dependency also appears, coming from the dep_generic crate
match_solution("gnat=" + e("8888.0.0 (gnat_native)") + ".*" +
               "gnat_native=" + e("8888.0.0") + ".*")

# Last combination is targeted x targeted
os.chdir("..")
init_local_crate("xxx_targeted_targeted")
run_alr("with", "--use=../dep_targeted")
alr_with("gnat_native")

# In this case the only possible solution is with the targeted compiler. The
# generic dependency no longer exists, as nobody requested a generic gnat.
match_solution("gnat_native=" + e("8888.0.0") + ".*")
p = run_alr("with", "--solve")
assert "gnat=" not in p.out, "Unexpected output"

print('SUCCESS')
