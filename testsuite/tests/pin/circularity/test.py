"""
Verify pin circularity detection
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
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
init_local_crate("yyy", enter=False)
init_local_crate("zzz")
alr_pin("yyy", path="../yyy", update=False)
os.chdir("..")
os.chdir("yyy")
alr_pin("zzz", path="../zzz", update=False)
os.chdir("..")
init_local_crate("xxx")
alr_pin("yyy", path="../yyy", update=False)

# At this point, xxx --> yyy --> zzz --> yyy
p = run_alr("pin", complain_on_error=False)
s = re.escape(dir_separator())
assert_match(
    ".*"
    "ERROR: Pin circularity detected when adding pin zzz --> yyy:\n"
    f"ERROR:    Last manifest in the cycle is .*{s}zzz{s}alire.toml\n",
    p.out)

print('SUCCESS')
