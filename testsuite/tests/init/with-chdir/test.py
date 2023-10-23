"""
Test "executable" only appears in --bin initializations
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import content_of

test_dir = os.getcwd()

# Binary crate
run_alr("init", "--bin", "xxx")
assert_match(".*executables = \[", content_of("xxx/alire.toml"))

# Check that it builds and runs
run_alr("--chdir=xxx", "run")

print('SUCCESS')
