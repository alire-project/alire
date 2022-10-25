"""
Tests that a failing build is reported during publishing
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

p = run_alr("publish", "crate.tgz",
            complain_on_error=False, force=True)
assert_match(".*Compilation failed.*", p.out)

print('SUCCESS')
