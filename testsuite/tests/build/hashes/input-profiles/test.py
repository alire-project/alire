"""
Test that the inputs to the hashing properly reflect the build profile
"""

import shutil
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.builds import find_hash, hash_input
from drivers.asserts import assert_match
from drivers import builds

run_alr("config", "--set", "--global", "dependencies.shared", "true")
init_local_crate()
alr_with("libhello")

# Build the crate in default mode, so dependencies are in RELEASE mode
run_alr("build")
hash1 = find_hash("libhello")
assert_match(".*profile:libhello=RELEASE.*",
             hash_input("libhello"))

# Build with dependencies in VALIDATION mode
# Clean up first because find_hash() will fail if there are multiple builds
shutil.rmtree(builds.path())
run_alr("build", "--profiles=*=validation")
hash2 = find_hash("libhello")
assert_match(".*profile:libhello=VALIDATION.*",
             hash_input("libhello"))

# Check that the hashes are different
assert hash1 != hash2, "Hashes should be different"


print("SUCCESS")
