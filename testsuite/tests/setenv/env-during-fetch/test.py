"""
Check that an env var is defined during dependency retrieval (get and with)
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator


def verify_output(text):
    assert_match('.*CHECKENV_TEST_VAR exists\n.*',
                 text, flags=re.S)

# The "checkenv" crate defines CHECKENV_TEST_VAR. Also, its executable prints
# "CHECKENV_TEST_VAR exists" or "CHECKENV_TEST_VAR does NOT exist" when run.
# The crate defines post-fetch actions to self-build and self-run, so the
# output is generated at the moment we want to check.

# Retrieve a crate that depends on checkenv: checkparent --> checkenv
p = run_alr("get", "checkparent")
verify_output(p.out)

# Create a crate from scratch and add the same dependency to perform the check
# during retrieval by `with`
run_alr("init", "--bin", "xxx")
os.chdir("xxx")
p = run_alr("with", "checkenv")
verify_output(p.out)

print('SUCCESS')
