"""
Check that an env var is defined during dependency retrieval (get and with)
"""

import os
import re
from glob import glob

from drivers import builds
from drivers.alr import run_alr
from drivers.asserts import assert_match


def verify_output(text, exists: bool):
    if exists:
        assert_match('.*CHECKENV_TEST_VAR exists\n.*',
                    text, flags=re.S)
    else:
        assert "CHECKENV_TEST_VAR exists" not in text, \
            "Unexpected output: " + text

# The "checkenv" crate defines CHECKENV_TEST_VAR. Also, its executable prints
# "CHECKENV_TEST_VAR exists" or "CHECKENV_TEST_VAR does NOT exist" when run.
# The crate defines post-fetch actions to self-build and self-run, so the
# output is generated at the moment we want to check.

# Retrieve a crate that depends on checkenv: checkparent --> checkenv
run_alr("get", "checkparent")
# Build the crate to trigger the post-fetch action
os.chdir(glob("checkparent*")[0])
p = run_alr("build", complain_on_error=False)
verify_output(p.out, exists=True)

# Create a crate from scratch and add the same dependency to perform the check
# during retrieval by `with`

run_alr("init", "--bin", "xxx")
os.chdir("xxx")
run_alr("with", "checkenv")
p = run_alr("build")
# For shared builds, the crate has been already fetched as a dependency of
# checkparent, so it should not emit anything. For sandboxed builds, it should
# emit as expected.
if builds.are_shared():
    verify_output(p.out, exists=False)
else:
    verify_output(p.out, exists=True)

print('SUCCESS')
