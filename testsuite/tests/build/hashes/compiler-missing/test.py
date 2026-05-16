"""
Check that when no compiler is available we cannot compute the build hash
"""


from drivers.alr import run_alr, init_local_crate, alr_settings_set
from drivers.asserts import assert_match

# The index in this test has no compilers configured; hence we cannot locate
# even the default external compiler.

# Init a crate without explicit compiler dependency
# This does not fail because hashes are not computed until build time
init_local_crate("xxx")

# A standalone crate can be built because the compiler isn't used for the root
# crate hash inputs
run_alr("build")

# A crate with only pinned dependencies can be built (since there is no build
# path for which the compiler version needs to be hashed)
run_alr("-C", "..", "init", "pinned_crate")
run_alr("with", "pinned_crate", "--use=../pinned_crate")
run_alr("build")

# Adding a shared dependency works because this doesn't yet trigger an
# update/build
run_alr("with", "libhello")

# The build fails because we cannot compute the dependency hash without a compiler
p = run_alr("build", complain_on_error=False)
assert_match(".*Unable to determine compiler version", p.out)

# Disable shared dependencies, and check that the build now succeeds
alr_settings_set("dependencies.shared", "false")
run_alr("build")

print("SUCCESS")
