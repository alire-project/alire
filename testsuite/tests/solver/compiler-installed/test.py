"""
Check that, for generic gnat dependencies, no compilers are installed (only a
locally available one is used).
"""

import subprocess
import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution

# With no compiler selected, the external compiler in the environment should be
# the only one available. We will verify this and capture its version (which is
# actually the version returned by `make`) for later use

# Verify only external compiler available
p = run_alr("toolchain")
assert_match(".*\n"  # Headers
             "gnat_external.*Available.*Detected.*\n",
             p.out)

# Capture version
version = re.search("gnat_external ([0-9.]+)", p.out, re.MULTILINE).group(1)

print(version)
# When no compiler is selected, since the external one is available, it should
# be used before offering to download a new compiler.

# Create a crate for our experiments
init_local_crate("xxx")

# Check that a generic dependency results in the external being used
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external) (shared)", escape=True)

# Check that requesting a version different to the one externally available
# results in missing compiler, as Alire won't try to install one.
alr_with("gnat", delete=True, manual=False)
alr_with(f"gnat/={version}")
match_solution(f"gnat/={version} (direct,hinted)", escape=True)
# Hinted because we know the crate exists as external

# Now, if the user installs a cross compiler, it will be used

run_alr("toolchain", "--install", "gnat_cross_2")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2) (shared)", escape=True)

# Likewise, if we install a native compiler, it will be preferred to a
# cross-compiler.

run_alr("toolchain", "--install", "gnat_native")
run_alr("update")
match_solution("gnat=8888.0.0 (gnat_native) (shared)", escape=True)

# If we remove the version exclusion, the external compiler will still be
# preferred as there is no selected compiler yet.

alr_with("gnat", delete=True, manual=False)
alr_with("gnat")
match_solution(f"gnat={version} (gnat_external) (shared)", escape=True)

# But, if the user selects a compiler as preferred, it will be used first

run_alr("config", "--set", "toolchain.use.gnat", "gnat_cross_2=7777.0.0")
run_alr("update")
match_solution("gnat=1.0.0 (gnat_cross_2) (shared)", escape=True)

# Finally, if the crate requests explicitly an uninstalled compiler, it will be
# downloaded, installed, and used before the rest of installed compilers.

alr_with("gnat_cross_1")
match_solution("gnat=9999.0.0 (gnat_cross_1) (shared)", escape=True)
match_solution("gnat_cross_1=9999.0.0 (shared)", escape=True)
# Verify it was actually installed
p = run_alr("toolchain")
assert_match(".*gnat_cross_1\s+9999.0.0\s+Available", p.out)

print('SUCCESS')
