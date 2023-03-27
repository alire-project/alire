"""
Check that the default "get & build" test for `alr test` works
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match, assert_in_file
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

# Check the build is performed in release mode
assert_in_file(os.path.join(glob("hello_1.0.1_*")[0], "config", "hello_config.gpr"),
               'Build_Profile : Build_Profile_Kind := "release";')

print('SUCCESS')
