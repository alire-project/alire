"""
Check that having two built-in test runners works properly
"""

import os
from drivers.alr import alr_manifest, init_local_crate, run_alr
from drivers.asserts import assert_substring

init_local_crate()

# Initialize two crates for the tests
init_local_crate("tests_1", enter=False)
init_local_crate("tests_2", enter=False)

# Create two built-in runners
with open(alr_manifest(), "a") as f:
    f.write("""
[[test]]
runner = 'alire'
directory = 'tests_1'

[[test]]
runner = 'alire'
directory = 'tests_2'
""")

# Run the tests and verify both runners ran properly
p = run_alr("test", quiet=False)
assert_substring("directory: tests_1", p.out)
assert_substring("directory: tests_2", p.out)
assert_substring("Successful test run", p.out)

# Test error when the test directory isn't a crate
os.remove("tests_1/alire.toml")
p = run_alr("test", complain_on_error=False)
assert_substring(
    "cannot detect a proper crate in test directory 'tests_1' "
    "(error: Could not detect an alire.toml manifest",
    p.out)

print("SUCCESS")
