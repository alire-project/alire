"""
Test deployment of a binary release and basic `alr install` use
"""

# NOTE: this test only runs on Linux

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match, assert_installed
from subprocess import run

import platform
import os

if platform.system() != "Linux":
    print('SUCCESS')
    exit(0)

PREFIX=os.path.join(os.getcwd(), "install")
PREFIX_ARG=f"--prefix={PREFIX}"

# Check that the prefix is empty
p = run_alr("install", "--info", PREFIX_ARG, quiet=False)
assert_match("There is no installation at prefix .*",
             p.out)

# Install the binary crate
p = run_alr("install", PREFIX_ARG, "crate", quiet=False)
assert_match("""Note: Computing solutions...
Success: Installation targets fully solved
Note: Deploying crate=1.0.0...
Note: Installing crate=1.0.0...
Success: Install to .* finished successfully in .* seconds.
""",
             p.out)

# Verify it's runnable at the expected place
p = run(f"{os.getcwd()}/install/bin/crate", capture_output=True)
assert p.returncode == 0, \
    f"Unexpected output, stdout: {p.stdout}, stderr: {p.stderr}"
assert_eq("Bin crate OK\n", p.stdout.decode())

# Verify release cannot be reinstalled
assert_match(".*Skipping already installed crate=1.0.0.*",
             run_alr("install", PREFIX_ARG, "crate",
                     quiet=False).out)

# Verify another version cannot be installed
assert_match(".*Release crate=0.1.0 has another version already installed:\n"
             ".*   crate=1.0.0.*",
             run_alr("install", PREFIX_ARG, "crate=0.1.0",
                     quiet=False, complain_on_error=False).out)

# Force install of the same crate and see no failure
run_alr("install", PREFIX_ARG, "crate=0.1.0", force=True)

# Recheck output
p = run(f"{os.getcwd()}/install/bin/crate", capture_output=True)
assert_eq("Bin crate OK\n", p.stdout.decode())

# Check contents of the prefix
assert_installed(PREFIX, ["crate=0.1.0"])

print('SUCCESS')