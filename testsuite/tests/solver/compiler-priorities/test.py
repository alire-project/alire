"""
Check compiler priorities in the solver. These priorities are:
    - The selected compiler, if defined
    - Newest installed native compiler
    - Newest installed cross-compiler
    - An externally available compiler
    - Newest uninstalled native compiler
    - Newest uninstalled cross-compiler
The previous is assuming a dependency on generic gnat=... For a target-specific
dependency, the non-matching compilers are skipped.
"""

import subprocess
import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution

# With no compiler selected, the external compiler in the environment should be
# the only one available. We will verify this and capture its version (which is
# actually the version returned by `make` for later use

# Verify only external compiler available
p = run_alr("toolchain")
assert_match(".*\n"  # Headers
             "gnat_external.*Available.*Detected.*\n",
             p.out)

# Capture version
version = re.search("[0-9.]+", p.out, re.MULTILINE).group()

# When no compiler is selected, since the external one is available, it should
# be used before offering to download a new compiler. We have a cross compiler
# with version 9999 that otherwise might be preferred, as it is newer.

# Create a crate for our experiments
init_local_crate("xxx")

# Check that a generic dependency results in the external being used
alr_with("gnat")
match_solution(".*" +
               re.escape(f"gnat={version} (gnat_external) (installed)") + ".*")

# Check that adding a second dependency on native packaged compiler is honored.
# Both dependencies should appear in the solution.
alr_with("gnat_native")
match_solution(
    ".*" +
    re.escape("gnat=2.0.0 (gnat_native) (installed)") + ".*" +
    re.escape("gnat_native=2.0.0 (installed) (origin: binary_archive)") + ".*")

# The previous dependency also should have caused the installation of the
# native compiler as an available compiler, which we will check:
p = run_alr("toolchain")
assert_match(".*gnat_native.*2.0.0.*Available.*",
             p.out)

# Move to a new crate
init_local_crate("yyy")

# Preinstall the v9999 compiler
run_alr("toolchain", "--install", "gnat=9999")
# Note also that we don't say the exact compiler to use, but the only one that
# provides that version is a cross-compiler

# Verify compiler availability
p = run_alr("toolchain")
assert_match(".*gnat_cross_1.*9999.*Available.*",
             p.out)

# Depend on any gnat. Since we have gnat_native=2 and gnat_cross_1=9999, normal
# version comparison would select the cross compiler, but native compilers take
# precedence. So the solution should match v2.
alr_with("gnat")
match_solution("gnat=2.0.0 (gnat_native)", escape=True)

# If we uninstall the native compiler, the cross compiler will be preferred to
# the external one:
run_alr("toolchain", "--uninstall", "gnat_native=2")

run_alr("update")
match_solution("gnat=9999.0.0 (gnat_cross_1)", escape=True)

# Let's reinstall the newest native compiler and verify the previous situation
run_alr("toolchain", "--install", "gnat_native")
p = run_alr("toolchain")
assert_match(".*gnat_native.*2.0.0.*Available.*",
             p.out)
run_alr("update")
match_solution("gnat=2.0.0 (gnat_native)", escape=True)

# We can force the use of the cross-compiler by selecting it as default:
run_alr("config", "--global",
        "--set", "toolchain.use.gnat", "gnat_cross_1=9999")
run_alr("update")
match_solution("gnat=9999.0.0 (gnat_cross_1) (installed)", escape=True)

# Since we cannot currently skip the external compiler, the last check is that
# a cross-compiler is retrieved when needed. Note that another cross-compiler
# is still selected as default, but since we need a different one, this setting
# is properly ignored in favor of the correct cross compiler.

init_local_crate("zzz")
alr_with("gnat")  # Will be solved with the selected cross compiler 1
match_solution("gnat=9999.0.0 (gnat_cross_1) (installed)", escape=True)

alr_with("gnat_cross_2")
# Now, this compiler should appear in the solution and be available, as it
# overrides the preferred compiler
match_solution("gnat_cross_2=1.0.0 (installed)", escape=True)

print('SUCCESS')
