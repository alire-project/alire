"""
Helper functions for the testing of shared builds
"""

from glob import glob
import os
from shutil import rmtree
import subprocess
from drivers.alr import alr_builds_dir, alr_workspace_cache, run_alr
from drivers.helpers import content_of


def clear_builds_dir() -> None:
    """
    Clear the shared build directory
    """
    rmtree(path())


def enable_shared() -> None:
    """
    Enable shared builds
    """
    run_alr("settings", "--global", "--set", "dependencies.shared", "true")


def are_shared() -> bool:
    """
    Return True if shared builds are enabled
    """
    try:
        return run_alr("settings", "--global", "--get",
                       "dependencies.shared").out.strip().lower() == "true"
    except:
        return False


def find_dir(crate_name: str) -> str:
    """
    Find the build dir of a crate in the shared build directory. It always uses
    forward slashes in the returned folder path.
    """
    if len(found := glob(f"{path()}/{crate_name}*/*")) != 1:
        raise AssertionError(f"Unexpected number of dirs for crate {crate_name}: {found}" + \
                             str(['\nINPUTS:\n' + content_of(os.path.join(f, "alire", "build_hash_inputs")) \
                                  for f in found])
                             )
    return glob(f"{path()}/{crate_name}*/*")[0].replace(os.sep, "/")


def find_hash(crate_name: str) -> str:
    """
    Find the hash of a crate in the shared build directory
    """
    return find_dir(crate_name).split("/")[-1]


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


def vault_path() -> str:
    """
    Return the path to the read-only release vault.
    """
    return os.path.join(path(), "..", "releases")


def deps_dir() -> str:
    """
    Return the path where dependencies are deployed: the release vault with
    shared builds, or the current workspace's cache otherwise.
    """
    return (vault_path() if are_shared()
            else os.path.join(alr_workspace_cache(), "dependencies"))


def flag(base: str, name: str) -> str:
    """
    Return the path to a flag file that alr keeps for the release whose top
    folder is base (a deployment, e.g. in the vault, or a build folder). The
    name is that of the file, as in the following helpers.
    """
    return os.path.join(base, "alire", "flags", name)


def complete_copy_flag(base: str) -> str:
    """
    Flag of a deployment or build folder copied completely
    """
    return flag(base, "complete_copy")


def post_fetch_flag(base: str) -> str:
    """
    Flag of a release whose post-fetch actions have run
    """
    return flag(base, "post_fetch_done")


def built_flag(base: str) -> str:
    """
    Flag of a shared build folder compiled since its sources last changed
    """
    return flag(base, "built")


def monorepo_data_flag(base: str) -> str:
    """
    Flag recording the origin of the checkout of a monorepo
    """
    return flag(base, "monorepo_data.toml")


def is_built(crate_name: str) -> bool:
    """
    Whether the shared build folder of a crate is marked as built
    """
    return os.path.isfile(built_flag(find_dir(crate_name)))


def sync() -> None:
    """
    Sync the shared build directory
    """
    run_alr("build", "--stop-after=generation")
    return


def sync_builds() -> None:
    sync()
