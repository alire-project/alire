"""
Test that maintainers provide a plausible email
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest('Loading .*hello_world-0.1.0.toml:.*maintainers:.*'
                        'Maintainers must have a valid email,'
                        ' but got: Mr. User\n')

print('SUCCESS')
