"""
Check that index manifests can load git releases with top-level mirrors.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


expected = """\
crate=1.0.0: Sample git crate
Origin: commit 0123456789abcdef0123456789abcdef01234567 from https://example.com/crate.git
Mirrors:
   https://mirror.example.com/crate.git
   https://mirror2.example.com/crate.git
Properties:
   Description: Sample git crate
   License: MIT
   Maintainer: any@bo.dy
   Maintainers_Logins: someone
   Name: crate
   Version: 1.0.0
"""

p = run_alr("show", "crate")
assert_eq(expected, p.out)


print("SUCCESS")
