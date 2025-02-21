"""
Verify proper feedback when trying to spawn a missing executable
"""

import shutil
from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_substring

EXE = "madeUpC0mmand"

# To be extra sure, check the command is not in path
assert shutil.which(EXE) is None

init_local_crate()

p = run_alr("exec", "--", "madeUpC0mmand", complain_on_error=False)

assert_substring("Executable not found in PATH when spawning", p.out)                 

print('SUCCESS')
