"""
Test that `alr index --del` expects an index name, not a position, and that
deleting a name that is not configured gives a clear error.
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Deleting an index name that is not configured: the error should explain that
# --del expects the name of a configured index (regression test for #2113, in
# which "Given index not found: 1" misleadingly suggested that indexes could
# be deleted by position).
p = run_alr("index", "--del", "no_such_index", complain_on_error=False)
assert_match(
    ".*ERROR: There is no configured index named 'no_such_index'\\..*",
    p.out
)

# Deleting by position is not supported: the same name-oriented error appears
p = run_alr("index", "--del", "1", complain_on_error=False)
assert_match(
    ".*ERROR: There is no configured index named '1'\\..*",
    p.out
)


print('SUCCESS')
