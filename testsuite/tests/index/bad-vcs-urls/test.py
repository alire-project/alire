"""
Test that VCS URLs that git, hg or svn could take as options or commands are
rejected in index manifests, while the usual forms of VCS URLs still load.
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_invalid_manifest

MANIFEST = "my_index/index/he/hello_world/hello_world-0.1.0.toml"

CONTENTS = """\
description = "\\"Hello, world!\\" demonstration project"
name = "hello_world"
version = "0.1.0"
licenses = "GPL-3.0-only"
maintainers = ["user@example.com"]
maintainers-logins = ["mylogin"]

[origin]
url = "{}"
commit = "0123456789abcdef0123456789abcdef01234567"
"""


def write_manifest(url: str):
    os.makedirs(os.path.dirname(MANIFEST), exist_ok=True)
    with open(MANIFEST, "w") as f:
        f.write(CONTENTS.format(url))


for url, reason in [
        ("git+ssh://-oProxyCommand=x/repo.git",
         "its user or host must not begin with '-'"),
        ("git+ssh://-oProxyCommand=x@host/repo.git",
         "its user or host must not begin with '-'"),
        ("hg+ssh://-oProxyCommand=x/repo",
         "its user or host must not begin with '-'"),
        ("git+ext::sh", "transport 'ext' is not supported"),
        ("git+fd::17", "transport 'fd' is not supported"),
        ("git+https://example.com/a repo.git",
         "it must not contain whitespace or control characters"),
        ("git+file:-repo", "it must not be empty or begin with '-'")]:
    write_manifest(url)
    assert_invalid_manifest(f"invalid VCS URL '{re.escape(url)}': "
                            + re.escape(reason))

# Usual forms still load

for url in ["git+https://github.com/example/hello_world.git",
            "https://github.com/example/hello_world.git",
            "git+ssh://git@github.com/example/hello_world.git",
            "git@github.com:example/hello_world.git",
            "git+git://example.com/hello_world.git",
            "hg+https://example.com/hello_world",
            "svn+https://example.com/hello_world"]:
    write_manifest(url)
    run_alr("show", "hello_world")

print("SUCCESS")
