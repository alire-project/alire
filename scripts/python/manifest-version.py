#!/usr/bin/env python3
# Find all crates in new index format and move origin-related fields to their own table
#
# The update is done in place and for all indexes found under the current directory.

import alire.index
import os
import rtoml

from alire import index, utils
from pathlib import Path

FROM_VERSION = "0.2"
INTO_VERSION = "0.3"


def fix_manifest(file):
    # file is the absolute path to a manifest
    print(f"   Patching {file}...")
    with open(file, "r") as orig:
        data = orig.read()
    with open(file, "w") as updated:
        updated.write(f'metadata-version = "{INTO_VERSION}"\n')
        updated.write(data)
    

alire.index.migrate_indexes(os.getcwd(), FROM_VERSION, INTO_VERSION, fix_manifest)
