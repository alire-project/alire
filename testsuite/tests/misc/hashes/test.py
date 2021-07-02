"""
Verify the recognition of all supported hash types
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

# Verify loading
p = run_alr("show", "crate")
assert_match(".*Origin.*with hashes "
             "sha256:.{64}, sha512:.{128}",
             p.out)

# Verify actual hash use. v1 of crate is correct, v2 contains bad hashes

# This can only succeed if all hashes match
p = run_alr("get", "crate=1")

# Verify that a hash mismatch is also detected
p = run_alr("get", "crate=2", complain_on_error=False)
assert_match(".*release integrity test failed.*", p.out)

print('SUCCESS')
