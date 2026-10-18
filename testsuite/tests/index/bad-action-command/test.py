"""
Test invalid action command
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .* actions command must'
                        ' be an array of string\(s\)\n')

print('SUCCESS')
