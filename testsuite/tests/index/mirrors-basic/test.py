"""
Check that index manifests can load source-archive releases with top-level
mirrors.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


expected = """\
crate=1.0.0: Sample crate
Origin: source archive crate-1.0.0.tar.gz at http://example.com/crate-1.0.0.tar.gz with hash sha512:deadbeef
Mirrors:
   source archive crate-1.0.0.tar.gz at http://mirror1.example.com/crate-1.0.0.tar.gz
   source archive crate-renamed.tar.gz at http://mirror2.example.com/crate-renamed.tar.gz
Properties:
   Description: Sample crate
   License: MIT
   Maintainer: any@bo.dy
   Maintainers_Logins: someone
   Name: crate
   Version: 1.0.0
"""

p = run_alr("show", "crate")
assert_eq(expected, p.out)


print("SUCCESS")
