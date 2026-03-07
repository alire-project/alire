"""
Check that case expressions are rejected for dynamic properties
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
    if prop in content_of(alr_manifest()):
        replace_in_file(alr_manifest(), prop, f"{prop}.'case(os)'.'...'")
    else:
        append_to_file(alr_manifest(), [f"{prop}.'case(os)'.'...' = 'whatever'"])
    # Types don't matter as the check happens before deserialization

    p = run_alr("show", complain_on_error=False)
    assert_substring("Dynamic expression not allowed", p.out)


# Static props
STATIC_PROPS = [
    "authors",
    "auto-gpr-with",
    "build-switches",
    "description",
    "gpr-externals",
    "licenses",
    "long-description",
    "maintainers",
    "maintainers-logins",
    "tags",
    "notes",
    "website",
]

for prop in STATIC_PROPS:
    check_prop(prop)

print("SUCCESS")
