"""
Check that arrays are rejected for singleton properties.
"""

import os
from drivers.alr import init_local_crate, run_alr, alr_manifest
from drivers.asserts import assert_substring
from drivers.helpers import append_to_file, content_of, replace_in_file
import shutil

init_local_crate()

# Make a copy of the original manifest
BACKUP = "alire.toml.bak"
shutil.copyfile(alr_manifest(), BACKUP)


def check_prop(prop:str) -> None:
    # Restore pristine manifest
    shutil.copyfile(BACKUP, alr_manifest())
    append_to_file(alr_manifest(), [f"[[{prop}]]"])
    # Types don't matter as the check happens before deserialization

    p = run_alr("show", complain_on_error=False)
    assert_substring("cannot be an array", p.out)


# Static props
SINGLE_PROPS = [
    "auto-gpr-with",
    "build-profiles",
    "build-switches",
    "configuration",
    "environment",
    "gpr-set-externals"
]

for prop in SINGLE_PROPS:
    check_prop(prop)

print("SUCCESS")
