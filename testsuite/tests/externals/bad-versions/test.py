"""
Check that external version detector doesn't crash on problematic versions we
have been seeing in the wild.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_substring


def check_version(crate_name, version: str):
    assert_substring(version,
                     run_alr("show", crate_name, "--external-detect").out)


check_version("crate1", "1.1.0+abc")
check_version("crate2", "1.1.0+.abc")

print("SUCCESS")
