"""
Check that a linked dependency is considered during solution traversal
"""

from glob import glob
import re
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match

run_alr("get", "libhello")
init_local_crate()
run_alr("with", f"--use={glob('../libhello_*')[0]}")
alr_with("hello") # Libhello is already linked

# Verify the solution is as expected
p = run_alr("with", "--solve", quiet=False)
assert_match(".*" + re.escape("""\
Dependencies (solution):
   hello=1.0.1 (origin: filesystem)
   libhello=1.0.0 (pinned) (origin: ../libhello_1.0.0_filesystem)
"""),
             p.out)

# Verify that linked libhello prevents visiting hello too soon. This depends on
# debug output which is not very nice but there's no simple alternative.
p = run_alr("-vv", "build", quiet=False)

assert_match(".*Round 1: SKIP not-ready hello=1.0.1"
             ".*Round 1: VISIT ready libhello=1.0.0"
             ".*Round 2: VISIT ready hello=1.0.1",
             p.out)

print('SUCCESS')
