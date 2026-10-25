"""
Test that the repository URL is given to `git clone` after a `--`, so that it
can never be taken as an option.
"""

import os

from drivers.alr import alr_settings_set, run_alr
from drivers.asserts import assert_match
from drivers.helpers import MockCommand, on_windows

# A `git` that fails on clone, so that alr reports the arguments it was given
GIT_SCRIPT = """\
import sys
if "clone" in sys.argv:
    sys.exit(1)
"""

alr_settings_set("msys2.install_dir", os.path.abspath("does_not_exist"),
                 builtin=on_windows())
with MockCommand("git", GIT_SCRIPT, "cmd_dir"):
    p = run_alr("get", "hello_world", complain_on_error=False)
    assert_match(r'.*Command \["git", "clone", "--recursive", .*"--", '
                 r'"https://example\.com/hello_world\.git", "[^"]+"\] '
                 r'exited with code 1',
                 p.out)

print("SUCCESS")
