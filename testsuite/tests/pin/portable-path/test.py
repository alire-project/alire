"""
Verify that, in windows, an absolute path is accepted but a relative one is
preferred to be given in portable format (forward slashes)
"""

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import on_windows

import os

# Dependency to be pinned with absolute path
init_local_crate(name="dep_absolute")
path_absolute = os.getcwd()
os.chdir("..")

# Dependency to be pinned with portable relative path
init_local_crate(name="dep_portable", enter=False)

# Dependency to be pinned with bad relative path
init_local_crate(name="dep_not_portable", enter=False)

# Dependent main crate
init_local_crate()

# Should not cause error
alr_pin("dep_absolute", path=path_absolute)

# Should not cause error
alr_pin("dep_portable", path="../dep_portable")

# Now the update should detect the bad path. This check is only useful on Win
if on_windows():
    alr_pin("dep_not_portable", path=r"..\dep_not_portable", update=False)
    p = run_alr("update", complain_on_error=False)
    assert_match(".*Pin relative paths must use "
                 "forward slashes to be portable.*",
                 p.out)

print('SUCCESS')
