"""
Test that environment variable names in index manifests are restricted to
those every shell accepts, as they reach the shell through `alr printenv`.
"""

from drivers.asserts import assert_invalid_manifest

assert_invalid_manifest("invalid environment variable name 'BAD-NAME': "
                        "it must contain only letters, digits and '_'")

print("SUCCESS")
