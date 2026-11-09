"""
A single invalid manifest must not break the whole index: normal commands skip
it with a warning and load the rest, while `alr index --check` still fails.
"""

import re

from drivers.alr import run_alr
from drivers.asserts import assert_match, assert_not_substring, assert_substring

# Manifests in the index, by the kind of failure they cause
INVALID_TOML = "all_bad-1.0.0.toml"        # Checked_Error, only release
INVALID_PROPERTY = "half_bad-2.0.0.toml"   # Checked_Error, other release valid
UNEXPECTED = "no_dir-1.0.0.toml"           # Constraint_Error (bad local path)

# Normal commands keep working, warning once about each invalid manifest

for attempt in ["full load", "with providers cache"]:
    p = run_alr("search", "--crates", quiet=False)
    # Search results are one crate per line: "<name>  <description>"
    found = re.findall(r"^(\w+)\s+Test crate$", p.out, re.M)
    assert found == ["good_crate", "half_bad"], f"Unexpected crates: {found}"
    for manifest in [INVALID_TOML, INVALID_PROPERTY, UNEXPECTED]:
        assert_match(f".*Warning: Skipping invalid index manifest .*{manifest}",
                     p.out)
    assert_not_substring("unexpected error", p.out)

# Warnings are not shown with -q
p = run_alr("search", "--crates")
assert_not_substring("Skipping invalid index manifest", p.out)

# The valid release of a partially invalid crate is still available
p = run_alr("show", "half_bad", quiet=False)
assert_match(".*half_bad=1.0.0", p.out)

p = run_alr("show", "all_bad", quiet=False, complain_on_error=False)
assert_substring("does not exist in the index", p.out)

# Strict loading still fails, cleanly even for unexpected exceptions

p = run_alr("index", "--check", complain_on_error=False)
assert_match(".*ERROR:.*" + INVALID_TOML, p.out)
assert_not_substring("unexpected error", p.out)

print("SUCCESS")
