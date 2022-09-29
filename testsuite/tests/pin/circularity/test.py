"""
Verify pin circularity detection
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, alr_with, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import on_windows, dir_separator

import os
import re
import shutil

# Obvious self-pinning detection
init_local_crate()
alr_pin("xxx", path=".", update=False)
p = run_alr("pin", complain_on_error=False)
assert_match(".*"
             "ERROR: Pin circularity detected when adding pin xxx --> xxx:\n"
             "ERROR:    Last manifest in the cycle is .*\n",
             p.out)


# A real cycle detection
os.chdir("..")    # back to top-level
shutil.rmtree("xxx")  # and restart from scratch

# Prepare a cycle
init_local_crate("xxx", enter=False)
init_local_crate("yyy", enter=False)
init_local_crate("zzz")
alr_pin("xxx", path="../xxx", update=False)
os.chdir("..")
os.chdir("yyy")
alr_pin("zzz", path="../zzz", update=False)
os.chdir("..")
os.chdir("xxx")
alr_pin("yyy", path="../yyy", update=False)

# At this point, xxx --> yyy --> zzz --> xxx
p = run_alr("pin", complain_on_error=False)
s = re.escape(dir_separator())
assert_match(
    ".*"
    "ERROR: Pin circularity detected when adding pin zzz --> xxx:\n"
    f"ERROR:    Last manifest in the cycle is .*{s}zzz{s}alire.toml\n",
    p.out)

# Verify that the buggy case that was reported does not happen again
# In this case, we have
# dep1 -> dep2 -> dep3 -> dep4; dep1 -> dep3; dep1 -> dep4; dep2 -> dep4

init_local_crate("dep4", enter=False)

init_local_crate("dep3")
alr_with("dep4", path="../dep4")

os.chdir("..")
init_local_crate("dep2")
alr_with("dep3", path="../dep3")
alr_with("dep4", path="../dep4")

os.chdir("..")
init_local_crate("dep1")
alr_with("dep2", path="../dep2")
alr_with("dep3", path="../dep3")
alr_with("dep4", path="../dep4")

p = run_alr("with", "--solve")
assert_eq("""\
Dependencies (direct):
   dep2*
   dep3*
   dep4*
Pins (direct):
   dep2 = { path='../dep2' }
   dep3 = { path='../dep3' }
   dep4 = { path='../dep4' }
Dependencies (solution):
   dep2=0.1.0-dev (pinned) (origin: ../dep2)
   dep3=0.1.0-dev (pinned) (origin: ../dep3)
   dep4=0.1.0-dev (pinned) (origin: ../dep4)
Dependencies (graph):
   dep1=0.1.0-dev --> dep2=0.1.0-dev (*)
   dep1=0.1.0-dev --> dep3=0.1.0-dev (*)
   dep1=0.1.0-dev --> dep4=0.1.0-dev (*)
   dep2=0.1.0-dev --> dep3=0.1.0-dev (*)
   dep2=0.1.0-dev --> dep4=0.1.0-dev (*)
   dep3=0.1.0-dev --> dep4=0.1.0-dev (*)
""",
          p.out)

print('SUCCESS')
