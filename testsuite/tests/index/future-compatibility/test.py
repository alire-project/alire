"""
Check that we can safely ignore known future entries in manifests
"""

import os

from drivers.alr import init_local_crate, run_alr, alr_manifest
from drivers.asserts import assert_substring

TELLTALE = "Discarding future property in manifest: future"

start_path = os.getcwd()

def test_future_property(crate_name, future_content):
    """Test that future properties are safely ignored with proper warning"""
    os.chdir(start_path)
    init_local_crate(crate_name)
    with open(alr_manifest(), "a") as f:
        f.write(future_content)

    p = run_alr("show", quiet=False)
    assert_substring(crate_name, p.out)
    assert_substring(TELLTALE, p.out)

# Test table
test_future_property("xxx", """
[future]
unknown = "property"
""")

# Test array
test_future_property("yyy", """
[[future]]
unknown = "property"
""")

# Test scalar
test_future_property("zzz", """
future = "is bright"
""")

# Ensure that actually unknown properties are still rejected

os.chdir(start_path)
init_local_crate("qqq")
with open(alr_manifest(), "a") as f:
    f.write("""
unknowns = "aren't scary"
""")
p = run_alr("show", quiet=False, complain_on_error=False)
assert_substring("invalid property: 'unknowns'", p.out)

print("SUCCESS")