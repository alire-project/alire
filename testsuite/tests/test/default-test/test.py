"""
Check that the default "get & build" test for `alr test` works
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import content_of
from glob import glob

# Enter an empty folder
os.mkdir("t")
os.chdir("t")

run_alr("test", "hello")  # Should not err

# Check test outcome
assert_match(".*" +
             re.escape("pass:hello=1.0.0") + ".*" +
             re.escape("pass:hello=1.0.1") + ".*",
             content_of(glob("*.txt")[0]))

print('SUCCESS')
