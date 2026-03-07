"""
Test that running `alr test` from a subdirectory works as expected.
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import mkcd

# Initialize a local crate with a test
init_local_crate("xxx", with_test=True)

# Create a subdirectory and run the test from there
mkcd("subdir")
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

# Create another level of subdirectory
mkcd("subsubdir")
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

# create a subcrate two levels down without a `test` section
init_local_crate("yyy", with_test=False)
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

# check that using `--legacy` does not run the testsuite from
# the toplevel crate
p = run_alr("test", "--legacy", quiet=False)
assert "[ PASS ]" not in p.out
assert_substring("Successful actions run", p.out)

print("SUCCESS")
