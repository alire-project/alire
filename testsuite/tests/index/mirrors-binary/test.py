"""
Check that a conditional (case) binary origin can carry mirrors. A mirror only
gives a url (its hashes come from the origin) and may itself be conditional or
flat (the `binary` marker is optional, inferred from the origin). `alr show`
renders conditional origins/mirrors as an indented multi-line case block.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


expected = """\
crate=1.0.0: Conditional binary crate with mirrors
Origin:
   case OS is
      when others => binary archive crate-1.0.0.tgz at http://example.com/crate-1.0.0.tgz
Mirrors:
   case OS is
      when others => binary archive crate-1.0.0.tgz at http://mirror1.example.com/crate-1.0.0.tgz
   binary archive crate-1.0.0.tgz at http://mirror2.example.com/crate-1.0.0.tgz
Properties:
   Description: Conditional binary crate with mirrors
   License: MIT
   Maintainer: any@bo.dy
   Maintainers_Logins: someone
   Name: crate
   Version: 1.0.0
"""

p = run_alr("show", "crate")
assert_eq(expected, p.out)


print("SUCCESS")
