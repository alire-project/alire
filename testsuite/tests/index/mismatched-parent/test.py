"""
Check detection of manifest in wrong parent
"""

from drivers.asserts import assert_invalid_manifest

# The file is skipped with a warning, while the search still works, and
# it is rejected by `alr index --check`
assert_invalid_manifest('Mismatch between manifest and parent:',
                        "search", "--crates", command_fails=False)

print('SUCCESS')
