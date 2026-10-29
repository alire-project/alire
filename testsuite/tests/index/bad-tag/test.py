"""
Test invalid characters in a tag are rejected
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .*hello_world-0.1.0.toml:.*tags:'
                        '.*Tag string is not valid: .*\n')

print('SUCCESS')
