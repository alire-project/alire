#!/usr/bin/env python3
# Find all crates in new index format and move origin-related fields to their own table
#
# The update is done in place and for all indexes found under the current directory.

import glob
import os
import rtoml

from alire import index, utils
from pathlib import Path

FROM_VERSION = "0.1"
INTO_VERSION = {"version": "0.2"}


def fix_url(url):
    # For good measure, replace incorrect local paths
    return url.replace("file://", "file:")


def fix_manifest(file):
    # file is the absolute path to a manifest
    print(f"   Patching {file}...")
    try:
        manifest = rtoml.load(Path(file))
    except rtoml.TomlParsingError as ex:
        print(f"FAILED to patch {file}: " + str(ex))
        raise

    if "origin" not in manifest:
        print(f"   WARNING: manifest without origin? {file}")
        return

    origin = dict()
    url = fix_url(manifest.pop("origin"))

    # Move any '@commit' info to a separate key
    origin["url"] = url.split('@')[0]
    if '@' in url:
        origin["commit"] = url.split('@')[1]

    if "origin-hashes" in manifest:
        origin["hashes"] = manifest.pop("origin-hashes")
    if "archive-name" in manifest:
        origin["archive-name"] = manifest.pop("archive-name")

    manifest["origin"] = origin

    # Rewrite patched manifest
    with open(file, "wt") as file:
        rtoml.dump(utils.fix_manifest_order(manifest), file)


def migrate(path):
    print(f"Migrating {path}...")

    # Visit all nested TOML files. We do not check for proper placement.
    for file in glob.iglob(os.path.join(path, '**/*.toml'), recursive=True):
        if "/index.toml" not in file and "external" not in file:
            fix_manifest(file)

    # Finalize by updating the index version
    with open(os.path.join(path, "index.toml"), "wt") as file:
        rtoml.dump(INTO_VERSION, file)


def traverse(path):
    if index.is_index(path, FROM_VERSION):
        migrate(path)
    else:
        # keep looking for nested indexes
        with os.scandir(path) as iterate:
            for child in iterate:
                if child.is_dir():
                    traverse(child.path)


traverse(os.getcwd())
