"""
Test that running `alr test` from a subdirectory other than the root and the
`tests` directory works as expected.
"""

import os
from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring

# Initialize a local crate with a test
init_local_crate("xxx", with_test=True)

# Run the test from the root directory to verify it works
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

# Create a subdirectory and run the test from there
os.makedirs("subdir", exist_ok=True)
os.chdir("subdir")

# Run the test from the subdirectory
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

# Create another level of subdirectory
os.makedirs("deeper", exist_ok=True)
os.chdir("deeper")

# Run the test from the deeper subdirectory
p = run_alr("test")
assert_substring("[ PASS ]", p.out)

print("SUCCESS")
