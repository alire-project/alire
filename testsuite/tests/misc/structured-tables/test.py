"""
Verify structured output of tables
"""

import json

import toml
from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq, assert_match

# Check a few commands that output tables

# Listing of crates
assert_eq("""\
[
  {
    "name": "gnat_external",
    "description": "GNAT is a compiler for the Ada programming language"
  },
  {
    "name": "gprbuild",
    "description": "Fake gprbuild external"
  }
]
""",
          run_alr("--format=JSON", "search", "--crates").out)

assert_eq("""\
[[data]]
description = "GNAT is a compiler for the Ada programming language"
name = "gnat_external"
[[data]]
description = "Fake gprbuild external"
name = "gprbuild"

""",
          run_alr("--format=TOML", "search", "--crates").out)

# Empty pin list

init_local_crate()
assert_eq("""\
[]
""",
          run_alr("--format=JSON", "pin").out)

assert_eq("""\

""",
          run_alr("--format=TOML", "pin").out)

# Check that objects can be reconstructed and queried from the output

for fmt in ["JSON", "TOML"]:
    # List of releases
    out = run_alr(f"--format={fmt}", "-q", "search", "--list", "--external").out
    # Load and adjust according to format
    if fmt == "TOML":
        releases = toml.loads(out)["data"]
        # In the TOML case, top-level is always a table with a single key "data"
    else:
        releases = json.loads(out)

    # Check that it is a list of the expected length
    assert_eq(list, type(releases))
    assert_eq(2, len(releases))
    # Check some data
    assert_eq("gnat_external", releases[0]["name"])


print("SUCCESS")
