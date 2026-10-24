"""
Test that paths relative to the crate in index manifests (project files,
executables, action and test directories) cannot point outside the deployed
origin. They may go up from the crate's subdir, as crates of a monorepo do to
share project files, but not above the origin root.
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_invalid_manifest

MANIFEST = "my_index/index/he/hello_world/hello_world-0.1.0.toml"

HEADER = """\
description = "\\"Hello, world!\\" demonstration project"
name = "hello_world"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["user@example.com"]
maintainers-logins = ["mylogin"]
"""

ARCHIVE_ORIGIN = """
[origin]
url = "https://example.com/hello_world.tar.gz"
hashes = ["sha512:deadbeef"]
"""

# A crate two levels below the root of its repository
GIT_ORIGIN = """
[origin]
url = "git+https://github.com/example/hello_world.git"
commit = "0123456789abcdef0123456789abcdef01234567"
subdir = "./crates/hello_world/"
"""

ESCAPES = "it must not go above the root of its origin"


def write_manifest(origin: str, fields: str):
    os.makedirs(os.path.dirname(MANIFEST), exist_ok=True)
    with open(MANIFEST, "w") as f:
        # Fields go before the origin, so they are top-level keys
        f.write(HEADER + fields + origin)


def rejected(origin: str, fields: str, what: str, path: str, reason: str):
    write_manifest(origin, fields)
    assert_invalid_manifest(f"invalid {what} '{re.escape(path)}': "
                            + re.escape(reason))


def accepted(origin: str, fields: str):
    write_manifest(origin, fields)
    run_alr("index", "--check")
    run_alr("show", "hello_world")


# Each field, going above the root of an archive origin (the crate root)

rejected(ARCHIVE_ORIGIN, 'project-files = ["../x.gpr"]\n',
         "project-files", "../x.gpr", ESCAPES)
rejected(ARCHIVE_ORIGIN, 'executables = ["../x"]\n',
         "executables", "../x", ESCAPES)
rejected(ARCHIVE_ORIGIN,
         '[[actions]]\ntype = "post-fetch"\ncommand = ["true"]\n'
         'directory = "../x"\n',
         "action directory", "../x", ESCAPES)
rejected(ARCHIVE_ORIGIN, '[test]\nrunner = "alire"\ndirectory = "../x"\n',
         "test directory", "../x", ESCAPES)

# Going up from a subdir, but not above the root

accepted(GIT_ORIGIN, 'project-files = ["../../gnat/shared.gpr"]\n')
accepted(GIT_ORIGIN, 'project-files = ["./a/../../b/./../gnat/x.gpr"]\n')
accepted(GIT_ORIGIN,
         '[[actions]]\ntype = "post-fetch"\ncommand = ["true"]\n'
         'directory = "../../tools"\n')

rejected(GIT_ORIGIN, 'project-files = ["../../../x.gpr"]\n',
         "project-files", "../../../x.gpr", ESCAPES)
rejected(GIT_ORIGIN, 'project-files = ["a/../../../../x.gpr"]\n',
         "project-files", "a/../../../../x.gpr", ESCAPES)

# Action directories may use either separator, so both are checked

rejected(GIT_ORIGIN,
         '[[actions]]\ntype = "post-fetch"\ncommand = ["true"]\n'
         "directory = '..\\..\\..\\x'\n",
         "action directory", "../../../x", ESCAPES)

# Values in every case are checked, not only those for this platform

rejected(GIT_ORIGIN,
         '[project-files."case(os)"]\nlinux = ["ok.gpr"]\n'
         'windows = ["../../../x.gpr"]\n"..." = ["ok.gpr"]\n',
         "project-files", "../../../x.gpr", ESCAPES)

# Other paths that are not portable

rejected(ARCHIVE_ORIGIN, 'project-files = ["/abs/x.gpr"]\n',
         "project-files", "/abs/x.gpr", "it must be a relative path")
rejected(ARCHIVE_ORIGIN, "project-files = ['gnat\\x.gpr']\n",
         "project-files", "gnat\\x.gpr", "it must use forward slashes")

print("SUCCESS")
