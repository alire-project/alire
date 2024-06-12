"""
Test the output of `alr get` for an abstract crate
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq

# gnat does not contain releases but several other crates provide it, so rather
# than saying that the crate doesn't exist, show its providers:

assert_eq("""Crate gnat is abstract and provided by:
   gnat_cross_1
   gnat_cross_2
   gnat_external
   gnat_native
""",
          run_alr("get", "gnat", "--dirname").out)

# Check the error for a truly unknown crate:

assert_eq("""\
ERROR: Crate [unobtanium] does not exist in the index.
""",
          run_alr("get", "unobtanium", "--dirname", complain_on_error=False).out)

print('SUCCESS')
