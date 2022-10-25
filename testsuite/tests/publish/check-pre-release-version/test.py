"""
Tests that pre-release version is reported during publish
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import init_git_repo

init_local_crate("my_crate")
p = run_alr("publish", "--tar",
            complain_on_error=False, quiet=False)

assert_match(".*The release version ends with '-dev'..*", p.out)

print('SUCCESS')
