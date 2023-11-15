"""
Check that, for generic gnat dependencies, uninstalled compilers are used only
as last resort, and a warning is shown
"""

import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_match, match_solution

# With no compiler selected, the external compiler in the environment should be
# the only one available. We will verify this and capture its version (which is
# actually the version returned by `make`) for later use

# Verify only external compiler available
p = run_alr("toolchain")
assert_match(".*\n"  # Headers
             "gnat_external.*Available.*Detected.*\n",
             p.out)

# We know the external version of the compiler (3.3.3)
version = "3.3.3"

# When no compiler is selected, since the external one is available, it should
# be used before offering to download a new compiler.

# Create a crate for our experiments
init_local_crate("xxx")

# Check that a generic dependency results in the external being used
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external)", escape=True)

# Check that requesting a version different to the one externally available
# results in a complete solution but with installation warning
alr_with("gnat", delete=True, manual=False)
p = run_alr("with", f"gnat/={version}", quiet=False, force=True)
assert_match(".*download of a toolchain not currently installed", p.out)
match_solution(f"gnat=8888.0.0 (gnat_native) (origin: binary_archive)",
               escape=True)

# Now, if the user installs a cross compiler, it will be used in preference to
# the 8888 newer one, because it's installed (but we need to uninstall the 8888
# one first)
run_alr("toolchain", "--uninstall", "gnat_native=8888")

run_alr("toolchain", "--install", "gnat_cross_2")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2)", escape=True)

# Likewise, if we install a native compiler, it will be preferred to a
# cross-compiler.

run_alr("toolchain", "--install", "gnat_native")
run_alr("update")
match_solution("gnat=8888.0.0 (gnat_native)", escape=True)

# If we remove the version exclusion, the external compiler will still be
# preferred as there is no selected compiler yet.

alr_with("gnat", delete=True, manual=False)
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external)", escape=True)

# But, if the user selects a compiler as preferred, it will be used first

run_alr("config", "--set", "toolchain.use.gnat", "gnat_cross_2=7777.0.0")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2)", escape=True)

# Finally, if the crate requests explicitly an uninstalled compiler, it will be
# downloaded, installed, and used before the rest of installed compilers.

alr_with("gnat_cross_1")
match_solution("gnat=9999.0.0 (gnat_cross_1)", escape=True)
match_solution("gnat_cross_1=9999.0.0", escape=True)
# Verify it was actually installed
p = run_alr("toolchain")
assert_match(".*gnat_cross_1\s+9999.0.0\s+Available", p.out)

print('SUCCESS')
