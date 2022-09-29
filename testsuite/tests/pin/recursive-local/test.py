"""
Verify that recursive pins work for local paths
"""

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq, assert_match

import os

#  We are going to setup xxx --> yyy --> zzz, where xxx and zzz live at the
#  same level, and yyy is at ./nest/yyy

init_local_crate(name="zzz", enter=False)

os.mkdir("nest")
os.chdir("nest")
init_local_crate(name="yyy")
alr_pin("zzz", path="../../zzz")

os.chdir("..")
os.chdir("..")
init_local_crate()
alr_pin("yyy", path="../nest/yyy")

# Should work properly
p = run_alr("pin")
assert_eq('yyy file:../nest/yyy\n'
          'zzz file:../zzz\n',
          p.out)

print('SUCCESS')
