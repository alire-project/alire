"""
Check that conditional tests can be properly processed and exported, both a
single test given as a table or multiple tests given as an array.
"""

import os
import shutil
from drivers.alr import alr_manifest, init_local_crate, init_local_crate, run_alr
from drivers.helpers import append_to_file
from drivers.asserts import assert_substring

CRATE="xxx"

for mode in ["table", "array"]:

    # Remove crate if it exists
    shutil.rmtree(CRATE, ignore_errors=True)

    init_local_crate(CRATE, with_test=False)

    # Add a conditional test
    test = ["[[test]]"] if mode == "array" else []
    test.extend([
                      "[test.'case(os)'.'...']",
                      "runner = 'alire'"
                  ])
    append_to_file(alr_manifest(), test)

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

    os.chdir("..")


print("SUCCESS")
