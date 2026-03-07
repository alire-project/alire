"""
Bug #1379: -h/--help must not trigger our help system after a "--" argument
"""

import re
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, assert_substring

# For no command, "--" with help afterwards should simply trigger an error

assert_match(".*" + re.escape("Unrecognized option '--' (global)"),
             run_alr("--", "--help", complain_on_error=False).out)

# For a command that doesn't understand "--", same:

assert_match(".*" + re.escape("Unrecognized option '--' (command/topic \"show\")"),
             run_alr("show", "--", "--help", complain_on_error=False).out)

# For `alr exec`, it should trigger the secondary command help and not ours. We
# verify the command being launched includes the "--help" option. Since `exec`
# requires a workspace, we enter one first.

init_local_crate()

assert shutil.which("alr_fake") is None

assert_substring("Executable not found in PATH when spawning", 
                 run_alr("exec", "--", "alr_fake", "--help",
                         complain_on_error=False).out)

print("SUCCESS")
