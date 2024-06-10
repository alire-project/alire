"""
Check that when no compiler is available we cannot compute the build hash
"""


from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match

# The index in this test has no compilers configured; hence we cannot locate
# even the default external compiler.

# Init a crate without explicit compiler dependency
# This does not fail because hashes are not computed until build time
init_local_crate("xxx")

# A standalone crate can be built because the compiler isn't used for the root
# crate hash inputs
run_alr("build")

# Adding a dependency works because this doen't yet trigger an update/build
run_alr("with", "libhello")

# The build fails because we cannot compute the dependency hash without a compiler
p = run_alr("build", complain_on_error=False)
assert_match(".*Unable to determine compiler version", p.out)

print("SUCCESS")
