"""
Test that the inputs to the hashing properly reflect the build profile and
other inputs.
"""

import shutil
from drivers.alr import alr_with, external_compiler_version, init_local_crate, run_alr
from drivers.builds import find_hash, hash_input
from drivers.asserts import assert_eq, assert_match
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

# Chech that the hash inputs contains exactly what we expect it to contain.
# This includes environment variables, GPR externals set or observed, build
# profile, compiler version.

assert_eq(
    'environment:TEST_ENV=myenv\n'           # plain env var set
    'external:TEST_FREEFORM_UNSET=default\n' # declared unset GPR external
    'external:TEST_GPR_EXTERNAL=gpr_ext_B\n' # declared set GPR external
    'external:TEST_UNDECLARED=used_by_another_crate\n' # modified GPR external
    'profile:libhello=VALIDATION\n'          # build profile
    f'version:gnat_external={external_compiler_version()}\n',
                                             # compiler version
    hash_input("libhello"))

print("SUCCESS")
