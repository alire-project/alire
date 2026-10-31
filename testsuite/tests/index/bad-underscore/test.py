"""
Test underscores are properly rejected in property keys
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('TOML keys should use hyphens instead of underscores,'
                        ' but found key: depends_on')

print('SUCCESS')
