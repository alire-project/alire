"""
Check compiler version in hashing input
"""

import sys

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_match, match_solution
from drivers.builds import clear_builds_dir, hash_input


def check_hash(signature: str) -> None:
    """
    Check that the given signature is present in the hash inputs
    """
    assert_match(f".*{signature}.*", hash_input("crate_real"))


run_alr("config", "--set", "--global", "dependencies.shared", "true")

# Select the default preferred compiler, in this index is gnat_native=8888
run_alr("toolchain", "--select")

# Init a crate without explicit compiler dependency
init_local_crate("xxx")
alr_with("crate_real")  # A regular crate in the index
run_alr("update")       # Ensure the hash inputs are written to disk

# Check the expected compiler is in the hash inputs
check_hash("version:gnat_native=8888.0.0")


# Next, check with an explicit compiler in the dependencies. Note that we give
# the virtual dependency, but the actual native one is used for the hash.

# Clear the build cache so we are able to locate the new hash
clear_builds_dir()
alr_with("gnat=7777")  # Downgrade the compiler with an explicit dependency
run_alr("update")

# Check the expected compiler is in the hash inputs
check_hash("version:gnat_native=7777.0.0")


# Finally, check that having two explicit dependencies on the compiler (one
# virtual and another real) does not cause a conflict in compiler detection.
# The same compiler as in the previous step should be used for the hash.

clear_builds_dir()
alr_with("gnat_native")
run_alr("update")
check_hash("version:gnat_native=7777.0.0")


print('SUCCESS')
sys.exit(0)
