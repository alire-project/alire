"""
Test that origins of unknown kinds in indexes are properly reported.
"""

from drivers.asserts import assert_invalid_manifest


# The manifest is skipped with a warning, and rejected by `alr index --check`
assert_invalid_manifest(' unsupported scheme .*\n')

print('SUCCESS')
