"""
Test working of `alr show --nested`
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match

import os

#  We create a crate below us; this should be detected

init_local_crate(enter=False)
assert_match(".*Found 1 nested crate in .*:\n"
             r"   xxx/xxx=0.1.0-dev: \(no description\)\n",
             run_alr("show", "--nested", quiet=False).out)

# After entering the crate, it is no longer nested and shouldn't be detected

os.chdir("xxx")
assert_match(r"\s*",
             run_alr("show", "--nested", quiet=False).out)

# If we initialize another crate without entering it, it should again be
# detected

init_local_crate(name="yyy", enter=False)
assert_match(".*Found 1 nested crate in .*:\n"
             r"   yyy/yyy=0.1.0-dev: \(no description\)\n",
             run_alr("show", "--nested", quiet=False).out)

print('SUCCESS')
