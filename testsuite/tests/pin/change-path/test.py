"""
Verify that the path of a pin can be changed and this is detected automatically
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import on_windows

import os


def create_dep(nest):
    """
    Create the same dependency yyy under the given nest path
    """
    os.mkdir(nest)
    os.chdir(nest)
    init_local_crate("yyy", enter=False)
    os.chdir("..")


# Two versions of the same crate
create_dep("nest1")
create_dep("nest2")

# Dependent main crate
init_local_crate()

alr_pin("yyy", path="../nest1/yyy")

# Edit the pin path by unpinning/repinning, without running alr in between,
# so this is equivalent to just editing the pin
alr_unpin("yyy", update=False)
alr_pin("yyy", path="../nest2/yyy", update=False)

# Now detect changes and show output
p = run_alr("pin", quiet=False)
assert_eq("""Note: Synchronizing workspace...
Dependencies automatically updated as follows:

   · yyy 0.1.0-dev (path=../nest2/yyy)

yyy file:../nest2/yyy\n""",
          p.out)

print('SUCCESS')
