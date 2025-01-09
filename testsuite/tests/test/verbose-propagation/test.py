"""
Check that when running `alr test` with the verbose flag, the spawned command
of the default test action inherits the verbosity flag.
"""

import os
from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_not_substring, assert_substring
from drivers.helpers import content_of

# Run `alr test` in a local crate for this test with increasing verbosity
# levels; we check the existence of expected output in the test log. The
# selected messages are representative of the log level at play.

LOGFILE = os.path.join("alire", "alr_test_local.log")

init_local_crate()

# Default log level
run_alr("test", quiet=False)
assert_not_substring("alr build done", content_of(LOGFILE))

# Verbose
run_alr("-v", "test", quiet=False)
assert_substring("alr build done", content_of(LOGFILE))
assert_not_substring("Setenv ALIRE=True", content_of(LOGFILE))

# More verbose
run_alr("-vv", "test", quiet=False)
assert_substring("alr build done", content_of(LOGFILE))
assert_substring("Setenv ALIRE=True", content_of(LOGFILE))

print("SUCCESS")
