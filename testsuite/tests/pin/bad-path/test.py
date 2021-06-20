"""
Verify that pinning a bad path is rejected
"""

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq, assert_match

init_local_crate()
alr_pin("badcrate", path="../bad/path", update=False)

# Now the update should detect the bad path
p = run_alr("update", complain_on_error=False)
assert_match(".*Pin path is not a valid directory:.*",
             p.out)

print('SUCCESS')
