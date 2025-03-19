"""
Test that running `alr test` from a subdirectory other than the root and the
`tests` directory works as expected.
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

print("SUCCESS")
