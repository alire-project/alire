"""
Verify that `alr clean --temp` works properly
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

import e3
import os

os.mkdir("test")
os.chdir("test")

# We create a temp file above us, one at the current dir, and one below.
# The one above us should not be cleaned. Also, a file not conforming to
# "alr-????.tmp" should not be cleaned either.

e3.os.fs.touch("../alr-0001.tmp")
e3.os.fs.touch("alr-0002.tmp")
os.mkdir("nested")
e3.os.fs.touch("nested/alr-0003.tmp")
e3.os.fs.touch("alien.tmp")

p = run_alr("clean", "--temp")

assert os.path.exists("../alr-0001.tmp"), "unexpected deletion"
assert os.path.exists("alien.tmp"), "unexpected deletion"
assert not os.path.exists("alr-0002.tmp"), "unexpected file"
assert not os.path.exists("nested/alr-0003.tmp"), "unexpected file"

# Finally verify that running again in a clean environment does nothing

p = run_alr("clean", "--temp", quiet=False)
assert_eq("No temporaries found.\n", p.out)

print('SUCCESS')
