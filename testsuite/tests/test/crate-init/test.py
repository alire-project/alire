"""
Test the skeleton tests crate created in `alr init`
"""

import os.path
from drivers.alr import run_alr
from drivers.asserts import assert_match

run_alr("init", "--lib", "xxx")
os.chdir("xxx")

p = run_alr("test", complain_on_error=False)
assert_match(".*\[ FAIL \] tests-example_test.*", p.out)
# default test after init always fails

print('SUCCESS')
