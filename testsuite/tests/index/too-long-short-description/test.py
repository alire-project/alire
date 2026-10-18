"""
Test that too long descriptions are rejected
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .*hello_world-0.1.0.toml:'
                        '.*description:.*Description string is too long'
                        ' \(must be no more than [0-9]+\)\n')

print('SUCCESS')
