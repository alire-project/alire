"""
Test that too long descriptions are rejected
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .*hello_world-0.1.0.toml:'
                        '.*description is too long: [0-9]+ characters,'
                        ' the maximum is 72\n')

print('SUCCESS')
