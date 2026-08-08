"""Dependency feature syntax requires an index new enough to parse it."""

from drivers.alr import run_alr
from drivers.asserts import assert_substring


p = run_alr("search", "--crates", complain_on_error=False)
assert p.status != 0, "the 1.4 index should have been rejected"
assert_substring("crate features require index version 1.5.0 or newer", p.out)

print("SUCCESS")
