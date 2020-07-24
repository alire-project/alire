#!/usr/bin/env python3
# Find all crates in old index format (single file per crate) and generate one
# file per release.
#
# The update is done in place and for all indexes found under the current
# directory.

import os
import re
import rtoml

from copy import deepcopy
from pathlib import Path

FROM_VERSION = "0.3"
INTO_VERSION = {"version": "0.4"}


def is_index(path):
    # Look for an "index.toml" file that contains a matching 'version = "x.x"'
    target = os.path.join(path, "index.toml")
    if not os.path.isfile(target):
        return False
    with open(target) as file:
        try:
            contents = rtoml.load(file)
            if "version" in contents:
                version = contents["version"]
                if version == FROM_VERSION:
                    return True
                else:
                    print(f"Version mismatch: {path} version is {version}, expected {FROM_VERSION}")
                    return False
            else:
                print(f"Malformed index file: no version found inside {path}")
        except rtoml.TomlParsingError:
            print(f"Not a target: {target} failed to load as TOML")
            raise


def fix_order(obj):
    delayed = dict()

    # extract tables/table arrays:
    for key in obj:
        val = obj[key]
        if isinstance(val, dict) or \
                (isinstance(val, list) and len(val) > 0 and isinstance(val[0], dict)):
            delayed[key] = val

    # reinsert at last position
    for key in delayed:
        obj.pop(key)
        obj[key] = delayed[key]


def write_externals(crate_dirname, name, general, external):
    print(f"      Writing externals for {name}...")
    crate = deepcopy(general)
    crate["name"] = name
    crate["external"] = external

    fix_order(crate)

    with open(os.path.join(crate_dirname, name + "-external.toml"), "wt") as file:
        rtoml.dump(crate, file)


def write_release(crate_dirname, name, general, version, release):
    print(f"      Writing release {name}={version}...")
    crate = deepcopy(general)
    crate["name"] = name
    crate["version"] = version

    # Ensure proper merging
    for key in release:
        if key in crate:
            if isinstance(crate[key], dict) and isinstance(release[key], dict):
                # Merge these dicts:
                crate[key].update(release[key])
            elif isinstance(crate[key], list) and isinstance(release[key], list):
                # Merge arrays
                crate[key] += release[key]
            else:
                raise RuntimeError(f"Key {key} is both in general and release sections in {crate_dirname} {version}")
        else:
            crate[key] = release[key]

    # Fix relative origins which now are one level deeper than before
    if "origin" in crate and "../" in crate["origin"]:
        crate['origin'] = crate['origin'].replace("../", "../../", 1)

    # Fix dictionary ordering to ensure atoms (or arrays of atoms) are before tables/arrays of tables. Otherwise, rtoml
    # does not do it for us, and we end with an invalid TOML serialization order (top-level atoms must come before any
    # tables/arrays).

    fix_order(crate)

    # Fix `depends-on` to be an array. No matter that dependencies are grouped.
    if "depends-on" in crate:
        deps = crate.pop("depends-on")
        crate["depends-on"] = [deps]

    with open(os.path.join(crate_dirname, name + f"-{version}.toml"), "wt") as file:
        rtoml.dump(crate, file)


def fix_ver(version):
    # Ensure the version has major.minor.patch numbers, for regularity, and that pre-release/build info is kept
    semver = r"^(?P<major>0|[1-9]\d*)(?:\.(?P<minor>0|[1-9]\d*))?(?:\.(?P<patch>0|[1-9]\d*))?" \
             r"(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|" \
             r"\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"
    parts = re.match(semver, version).groupdict()
    return parts["major"] + \
           (("." + parts["minor"]) if parts["minor"] is not None else ".0") + \
           (("." + parts["patch"]) if parts["patch"] is not None else ".0") + \
           (("-" + parts["prerelease"]) if parts["prerelease"] is not None else "") + \
           (("+" + parts["buildmetadata"]) if parts["buildmetadata"] is not None else "")


def split_crate(parent_dir, crate_file):
    # crate_file is an os.DirEntry that points to an old-style merged manifests file
    print(f"   Splitting {crate_file.path}...")
    try:
        crates = rtoml.load(Path(crate_file.path))
        name = crate_file.name.split('.')[0]
        crate_dirname = os.path.join(parent_dir, name)
        if not os.path.isdir(crate_dirname):
            os.mkdir(crate_dirname)

        general = crates.pop("general")

        if "external" in crates:
            write_externals(crate_dirname, name, general, crates.pop("external"))

        # remaining top-level tables must be versions (general was already pop'd)
        for version in crates:
            write_release(crate_dirname, name, general, fix_ver(version), crates[version])

        # Clean up unneeded merged manifest
        os.remove(crate_file)
    except rtoml.TomlParsingError as ex:
        print(f"FAILED to migrate {crate_file}: " + str(ex))
        raise


def split_contents(path):
    # Path points to a two-letter folder containing merged manifests
    with os.scandir(path) as contents:
        for entry in contents:
            if entry.name.endswith(".toml"):
                split_crate(path, entry)
            else:
                print(f"EXTRANEOUS entry looking for manifests: {entry.path}")


def migrate(path):
    print(f"Migrating {path}...")

    # All two-letter folders will contain crates...
    with os.scandir(path) as contents:
        for entry in contents:
            if len(entry.name) == 2 and entry.is_dir():
                split_contents(entry.path)
            elif entry.name != "index.toml":
                print(f"EXTRANEOUS entry looking for shelves: {entry.path}")

    # Finalize by updating the index version
    with open(os.path.join(path, "index.toml"), "wt") as file:
        rtoml.dump(INTO_VERSION, file)


def traverse(path):
    if is_index(path):
        migrate(path)
    else:
        # keep looking for nested indexes
        with os.scandir(path) as iterate:
            for child in iterate:
                if child.is_dir():
                    traverse(child.path)


traverse(os.getcwd())
