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
os.chdir("xxx")
run_alr("run")

# Library crate must not provide an executable
os.chdir("..")
run_alr("init", "--lib", "yyy")
assert ".*executables = [" not in content_of("xxx/alire.toml"), \
    "Unexpected contents in manifest"

# Check the default executable is not built/runnable
os.chdir("yyy")
p = run_alr("run", complain_on_error=False)
assert_match(".*Executable .* not found.*", p.out)

print('SUCCESS')
