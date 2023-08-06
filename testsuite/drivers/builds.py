"""
Helper functions for the testing of shared builds
"""

from glob import glob
import os
from drivers.alr import alr_builds_dir


def find_dir(crate_name: str) -> str:
    """
    Find the build dir of a crate in the shared build directory
    """
    if len(found := glob(f"{path()}/{crate_name}_*")) != 1:
        raise AssertionError(f"Unexpected number of dirs for crate {crate_name}: {found}")
    return glob(f"{path()}/{crate_name}_*")[0]


def find_hash(crate_name: str) -> str:
    """
    Find the hash of a crate in the shared build directory
    """
    return find_dir(crate_name).split("_")[-1]


def hash_input(crate_name: str, as_lines: bool=False) -> str:
    """
    Return the hash inputs for a crate build dir
    """
    with open(os.path.join(f"{find_dir(crate_name)}",
                           "alire",
                           "build_hash_inputs")) as f:
        return f.readlines() if as_lines else f.read()


def path() -> str:
    """
    Return the path to the shared build directory.
    """
    return alr_builds_dir()