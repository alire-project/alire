"""
Check detection of bad file in index
"""

from drivers.asserts import assert_invalid_manifest

# The file is skipped with a warning, while the search still works, and
# it is rejected by `alr index --check`
assert_invalid_manifest('Unexpected file in index:.*shouldnt_be_here',
                        "search", "--crates", command_fails=False)

print('SUCCESS')
