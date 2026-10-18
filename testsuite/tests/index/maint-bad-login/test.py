"""
Test that maintainers-logins values can't be empty strings
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .*hello_world-0.1.0.toml:'
                        '.*maintainers-logins:.*'
                        'maintainers-logins values must be non-empty\n')

print('SUCCESS')
