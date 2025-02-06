"""
Verify proper URL given in help for `alr publish`, matching alr version or
master branch.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_substring

version = run_alr("--version").out.split()[1]
tag = "master" if '-' in version else f"v{version}"

assert_substring(f"/{tag}/doc/publishing.md",
                 run_alr("publish", "--help").out)

print("SUCCESS")
