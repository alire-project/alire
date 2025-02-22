"""
Verify that `alr --format=yam` does not crash
"""

from drivers.alr import run_alr
from drivers.asserts import assert_substring


p = run_alr("--format=yam", complain_on_error=False)  # Note, not yaml

# The invalid format should be properly reported instead of a exception being raised

assert_substring("Unknown argument in --format=yam", p.out)


print("SUCCESS")
