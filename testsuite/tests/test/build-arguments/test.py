"""
Run tests with --build-only and --build-args
"""

import os

from drivers.asserts import assert_file_exists, assert_not_substring, assert_substring
from drivers.alr import init_local_crate, run_alr
from drivers.helpers import exe_name, replace_in_file

init_local_crate("xxx", with_test=True)

p = run_alr("test", "--build-only")
assert_not_substring("PASS", p.out)
assert_not_substring("FAIL", p.out)
assert_file_exists(exe_name("tests/bin/xxx_tests-assertions_enabled"))

p = run_alr("test", "--build-only", "-B", "--version", quiet=False)
assert_substring("This is free software", p.out)

print("SUCCESS")
