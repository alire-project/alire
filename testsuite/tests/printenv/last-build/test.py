"""
Test that `alr printenv --last-build` works as expected.
"""

from drivers import builds
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_match

builds.enable_shared()

init_local_crate()
alr_with("libhello")
run_alr("build")

# After a default build, we obtain the hash of the build in release mode
hash_release = builds.find_hash("libhello")

builds.clear_builds_dir()

# Now obtain the hash of the build in development mode
run_alr("build", "--profiles=*=development")
hash_devel = builds.find_hash("libhello")

assert hash_release != hash_devel, "Hashes should be different"

# Check default printenv behavior
p = run_alr("printenv")
assert_match(f".*LIBHELLO_ALIRE_PREFIX=[^\n]*{hash_release}", p.out)

# Check printenv --last-build behavior
p = run_alr("printenv", "--last-build")
assert_match(f".*LIBHELLO_ALIRE_PREFIX=[^\n]*{hash_devel}", p.out)

print("SUCCESS")
