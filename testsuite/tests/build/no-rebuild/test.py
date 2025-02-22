"""
Ensure that no rebuild happens when `alr build` is invoked twice. Reports like
https://github.com/alire-project/alire/issues/1798 hint at a problem in gprbuild
itself, but this should not arise at least on simple crates.

This test differs from crate_config/no-rebuilds in that here we have dependencies.
"""

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import exe_name

# Basic check in which building twice should not trigger a rebuild

init_local_crate()
alr_with("libhello")
run_alr("build")
p = run_alr("build", quiet=False)
assert_substring (f'gprbuild: "{exe_name("xxx")}" up to date', p.out)

# Check that removing and re-adding a dependency does not trigger a rebuild
# (since the build folders already exist).

run_alr("with", "--del", "libhello")
run_alr("with", "libhello")
p = run_alr("build", quiet=False)
assert_substring (f'gprbuild: "{exe_name("xxx")}" up to date', p.out)


print("SUCCESS")
