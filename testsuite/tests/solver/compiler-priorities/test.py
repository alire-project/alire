"""
Check compiler priorities in the solver. These priorities are:
    - The selected compiler, if defined
    - An externally available compiler
    - Newest installed native compiler
    - Newest installed cross-compiler
    - Newest uninstalled explicit native compiler
    - Newest uninstalled explicit cross-compiler
Generic dependencies on gnat= cause compiler installation as a last resort.
Those prefer first a native compiler. This case is out of scope of this test.
"""

import re

from drivers.alr import run_alr, init_local_crate, alr_with, unselect_compiler
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
version = re.search("gnat_external ([0-9.]+)", p.out, re.MULTILINE).group(1)

# When no compiler is selected, only the gnat_external one should be used
# unless a targeted compiler dependency is used

# Create a crate for our experiments
init_local_crate("xxx")

# Check that a generic dependency results in the external being used
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external)", escape=True)

# Check that adding a second dependency on native packaged compiler is honored.
# Both dependencies should appear in the solution.
alr_with("gnat_native")
match_solution("gnat=8888.0.0 (gnat_native)", escape=True)
match_solution("gnat_native=8888.0.0", escape=True)

# The previous dependency also should have caused the installation of the
# native compiler as an available compiler, which we will check:
p = run_alr("toolchain")
assert_match(".*gnat_native.*8888.0.0.*Available.*",
             p.out)

# Move to a new crate
init_local_crate("yyy")

# Preinstall the v9999 compiler but keep no default selected. Users can achieve
# this by selecting "None" in the assistant, we do it manually through config.
run_alr("toolchain", "--select", "gnat=9999")
unselect_compiler()

# Note also that we don't say the exact compiler to use, but the only one that
# provides that version is a cross-compiler

# Verify compiler availability
p = run_alr("toolchain")
assert_match(".*gnat_cross_1.*9999.*Available.*",
             p.out)

# Depend on any gnat. Since no default is selected, the external one is used,
# even if other installed compilers are newer (cross_2=9999)
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external)", escape=True)

# Depend on any gnat but the externally available. Since we have gnat_native=8888
# and gnat_cross_1=9999, normal version comparison would select the cross
# compiler, but native compilers take precedence. So the solution should
# match v2.
alr_with("gnat", delete=True, manual=False)
alr_with(f"gnat/={version}")
match_solution("gnat=8888.0.0 (gnat_native)", escape=True)

# We can force the use of the cross-compiler by selecting it as default:
run_alr("toolchain", "--select", "gnat_cross_1=9999")
run_alr("update")
match_solution("gnat=9999.0.0 (gnat_cross_1)", escape=True)

# Check that a targeted compiler is retrieved when needed. Note that another
# cross-compiler is still selected as default, but since we need a different
# one, this setting is properly ignored in favor of the correct cross compiler.

init_local_crate("zzz")
alr_with("gnat")  # Will be solved with the selected cross compiler 1
match_solution("gnat=9999.0.0 (gnat_cross_1)", escape=True)

alr_with("gnat_cross_2")
# Now, this compiler should appear in the solution and be available, as it
# overrides the preferred compiler
match_solution("gnat_cross_2=1.0.0", escape=True)

print('SUCCESS')
