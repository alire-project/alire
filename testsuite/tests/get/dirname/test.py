"""
Verify output of `alr get --dirname`
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

assert_eq("libhello_1.0.0_filesystem\n",
          run_alr("get", "libhello", "--dirname").out)

print('SUCCESS')
