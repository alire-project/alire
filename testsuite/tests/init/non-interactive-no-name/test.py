"""Check that `alr -n init` yields an error, not an infinite loop."""

from drivers.alr import run_alr
from drivers.asserts import assert_substring

p = run_alr("init", complain_on_error=False)
assert_substring(
    "Crate name required (must be supplied as an argument in non-interactive mode)",
    p.out,
)

print("SUCCESS")
