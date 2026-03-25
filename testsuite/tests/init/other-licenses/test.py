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
    "MIT", "custom-abc", "LicenseRef-abc",
    "MIT WITH DocumentRef-foo:AdditionRef-bar"
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
    # Empty string selects the default value (which is also the empty string)
    default_msg = r"Using default: ''\r?\n" if license_str == "" else ""
    # Make sure the invalid expression error is shown only once
    # (https://github.com/alire-project/alire/issues/2069)
    invalid_msg = re.escape(f"Invalid SPDX license expression '{license_str}': ")
    expected_out = (
        rf"^{re.escape(license_str)}\r?\n{default_msg}{invalid_msg}"
        rf"(?!.*{invalid_msg}).*> "
    )
    # Run interactively
    run_alr_interactive(["init", "--bin", "xxx"],
                        output=(
                            ["> " for _ in range(6)]
                            + [expected_out]
                            + ["> " for _ in range(2)]
                        ),
                        input=["",          # Description
                               "",          # Full user name
                               "",          # Github login
                               "",          # Email
                               "9",         # License (select "Other...")
                               license_str, # License string (invalid)
                               "MIT",       # License string (valid this time)
                               "",          # Tags
                               ""],         # Website
                        timeout=3)

    # Prepare for next iteration
    shutil.rmtree("xxx")


# License expressions for which the "LicenseRef-" prefix should be suggested
custom_licenses = [
    "test", "ABCabc012.345-678"
]
for license_str in custom_licenses:
    suggested_license = f"LicenseRef-{license_str}"
    suggestion_msg = re.escape(f"Did you mean '{suggested_license}'?")
    # Run interactively
    run_alr_interactive(["init", "--bin", "xxx"],
                        output=(
                            ["> " for _ in range(6)]
                            + [suggestion_msg]
                            + ["> "]
                            + [suggestion_msg]
                            + ["> " for _ in range(2)]
                        ),
                        input=["",          # Description
                               "",          # Full user name
                               "",          # Github login
                               "",          # Email
                               "9",         # License (select "Other...")
                               license_str, # License string (invalid)
                               "n",         # Reject suggestion the first time
                               license_str, # Re-enter the same license string
                               "y",         # Accept suggestion this time
                               "",          # Tags
                               ""],         # Website
                        timeout=3)

    # Check that it can be shown, which will load the manifest
    os.chdir("xxx")
    p = run_alr("show")

    # Check that the manifest and the output of `alr show` actually contain the
    # selected license
    assert_substring(f'licenses = "{suggested_license}"', content_of("alire.toml"))
    assert_substring(f"License: {suggested_license}", p.out)

    # Prepare for next iteration
    os.chdir("..")
    shutil.rmtree("xxx")


print('SUCCESS')
