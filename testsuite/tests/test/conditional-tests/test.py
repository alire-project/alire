"""
Check that conditional tests can be properly processed and exported
"""

from drivers.alr import alr_manifest, init_local_crate, init_local_crate, run_alr
from drivers.helpers import append_to_file
from drivers.asserts import assert_substring

init_local_crate(with_test=False)

# Add a conditional test
append_to_file(alr_manifest(),
               [
                   "[[test]]",
                   "[test.'case(os)'.'...']",
                   "runner = 'alire'"
               ])

# Initialize the test nested crate
init_local_crate("tests", enter=False)

# Verify the test runs in all platforms
p = run_alr("test")
assert_substring("[ PASS ] tests", p.out)

# Verify the crate can be shown after resolving conditional expressions (this is
# equivalent to exporting the crate). `--system` is the key flag to force evaluation.
p = run_alr("--format=YAML", "show", "--system")
assert_substring("""\
"test":
  - "directory": "tests"
    "jobs": 0
    "runner": "alire"
""", p.out)


print("SUCCESS")
