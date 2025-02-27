"""
Verify output of `--format show --external` doesn't crash and is as expected.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

assert_eq("""\
[
  {
    "kind": "Executable",
    "description": "echo 1.0",
    "details": "([\\\\d\\\\.]+).*",
    "available": "True"
  }
]
""",
run_alr("--format", "show", "gnat_external", "--external").out)

print("SUCCESS")
