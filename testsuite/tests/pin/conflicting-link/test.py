"""
Verify that two equal links for the same create are accepted but two different
ones are rejected
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_eq, assert_match

import os

#  We are going to setup xxx --> yyy --> zzz, xxx --> zzz to verify same link
#  is accepted.

init_local_crate(name="zzz", enter=False)

os.mkdir("nest")  # By nesting yyy, we force different relative paths that
os.chdir("nest")  # should not be a problem as internally is the same abs path.
init_local_crate(name="yyy")
alr_pin("zzz", path="../../zzz")

os.chdir("..")
os.chdir("..")
init_local_crate()  # This places us at ./xxx
alr_pin("yyy", path="../nest/yyy")
alr_pin("zzz", path="../zzz")  # This is the doubly-linked same crate

# Should work properly
p = run_alr("pin")
assert_eq('yyy file:../nest/yyy\n'
          'zzz file:../zzz\n',
          p.out)

#  Now we will pin a different zzz from xxx,
#  so xxx --> ./xxx/zzz conflicts with ./nest/yyy --> ../../zzz

alr_unpin("zzz")
init_local_crate(name="zzz", enter=False)  # This one is at ./xxx/zzz
alr_pin("zzz", path="./zzz", update=False)

# Should detect conflicting links
p = run_alr("pin", complain_on_error=False)
assert_match(".*Conflicting pin links for crate zzz:.*",
             p.out)

print('SUCCESS')
