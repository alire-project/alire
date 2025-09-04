"""
Check that we can safely ignore the 3.0 built-in testsuite
"""

from drivers.alr import init_local_crate, run_alr, alr_manifest
from drivers.asserts import assert_substring

TELLTALE = "Discarding future feature in manifest: built-in testsuite"

init_local_crate()

# Manually write the testsuite table
with open(alr_manifest(), "a") as f:
    f.write("""
[test]
runner = "alire"
""")

assert_substring(TELLTALE, run_alr("show", quiet=False).out)

# Same with an array

init_local_crate("yyy")
with open(alr_manifest(), "a") as f:
    f.write("""
[[test]]
runner = "alire"
""")
p = run_alr("show", quiet=False)
assert_substring("yyy", p.out)
assert_substring(TELLTALE, p.out)


print("SUCCESS")
