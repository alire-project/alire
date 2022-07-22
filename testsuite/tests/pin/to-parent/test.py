"""
Check that a pin to the parent crate results in a simple ".." relative path.
This is for the common case of testing/demo subcrates
"""

from drivers.alr import run_alr, init_local_crate, alr_pin, alr_manifest
from drivers.helpers import lines_of, content_of
from shutil import rmtree

import os


def try_path(path: str):
    """
    Pin the parent crate using the given path, that must be equivalent to ".."
    """

    if os.path.isdir("child"):
        rmtree("child")

    init_local_crate("child")
    alr_pin("parent", path=path, manual=False)
    # This should result in the pin being simplified to ".."

    assert "parent = { path='..' }\n" in lines_of(alr_manifest()), \
        "Unexpected manifest contents: " + content_of(alr_manifest())

    os.chdir("..")  # We entered during init_local_crate()


init_local_crate("parent")  # We enter it by default

# Try some variants
try_path("..")
try_path("../")
try_path("../../parent")
try_path("src/../..")
try_path("src/../../../parent")

print('SUCCESS')
