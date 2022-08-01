"""
Test pinning to a version
"""

from drivers.alr import run_alr, alr_pin, alr_with, init_local_crate
from drivers.asserts import assert_eq, assert_match

init_local_crate()
alr_with("hello")

# Test pinning to the version currently in the solution
alr_pin("hello", manual=False)
p = run_alr("pin")
assert_eq("hello 1.0.1\n",
          p.out)

# Test pinning to a different explicit version
alr_pin("hello", version="1.0", manual=False, force=True)
p = run_alr("pin")
assert_eq("hello 1.0.0\n",
          p.out)

# Test that trying to use a non-equal restriction fails
p = run_alr("pin", "hello^1.0", complain_on_error=False)
assert_eq("ERROR: Plain crate name or crate=version argument expected"
          " for pinning, but got: hello^1.0\n",
          p.out)

# Test that pinning to a non-existent version is doable but solution is missing
p = run_alr("pin", "hello=7.7.7", force=True)
p = run_alr("pin")
assert_eq("hello 7.7.7\n", p.out)
p = run_alr("with", "--solve")
assert_eq("""\
Dependencies (direct):
   hello*
Pins (direct):
   hello = { version='7.7.7' }
Dependencies (external):
   hello=7.7.7 (direct,missed:unavailable,pin=7.7.7) (pinned)
Dependencies (graph):
   xxx=0.1.0-dev --> hello*
""", p.out)

print('SUCCESS')
