"""
Check that the user can specify valid SPDX license expressions
"""


import os
import re
import shutil

from drivers.alr import run_alr, run_alr_interactive
from drivers.asserts import assert_substring
from drivers.helpers import content_of


# License expressions which should be accepted
valid_licenses = [
    "MIT", "custom-abc",
]
for license_str in valid_licenses:
    # Run interactively
    run_alr_interactive(["init", "--bin", "xxx"],
                        output=["> " for _ in range(8)],
                        input=["",          # Description
                               "",          # Full user name
                               "",          # Github login
                               "",          # Email
                               "9",         # License (select "Other...")
                               license_str, # License string
                               "",          # Tags
                               ""],         # Website
                        timeout=3)

    # Check that it can be shown, which will load the manifest
    os.chdir("xxx")
    p = run_alr("show")

    # Check that the manifest and the output of `alr show` actually contain the
    # selected license
    assert_substring(f'licenses = "{license_str}"', content_of("alire.toml"))
    assert_substring(f"License: {license_str}", p.out)

    # Prepare for next iteration
    os.chdir("..")
    shutil.rmtree("xxx")


# License expressions which should be rejected entirely
bad_licenses = [
    "", "/", "test:test", "MIT WITH test"
]
for license_str in bad_licenses:
    # Make sure the invalid expression error is shown only once
    # (https://github.com/alire-project/alire/issues/2069)
    invalid_msg = re.escape(f"Invalid SPDX license expression '{license_str}': ")
    invalid_re = rf"^{re.escape(license_str)}\r?\n{invalid_msg}(?!.*{invalid_msg})"
    # Run interactively
    run_alr_interactive(["init", "--bin", "xxx"],
                        output=(
                            ["> " for _ in range(6)]
                            + [invalid_re]
                            + ["> " for _ in range(3)]
                        ),
                        input=["",          # Description
                               "",          # Full user name
                               "",          # Github login
                               "",          # Email
                               "9",         # License (select "Other...")
                               license_str, # License string
                               "MIT",       # License string (valid this time)
                               "",          # Tags
                               ""],         # Website
                        timeout=3)

    # Prepare for next iteration
    shutil.rmtree("xxx")


print('SUCCESS')
