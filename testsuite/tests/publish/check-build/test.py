"""
Tests that a failing build is reported during publishing
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

p = run_alr("publish", "--force", "my_index/crates/crate.tgz",
            complain_on_error=False)
assert_match(".*Compilation failed.*", p.out)

print('SUCCESS')
