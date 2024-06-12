"""
A test that uses an index in testsuite/fixtures
"""

from drivers.alr import run_alr
# from drivers.asserts import assert_eq, assert_match


p = run_alr("clean")


print("SUCCESS")
