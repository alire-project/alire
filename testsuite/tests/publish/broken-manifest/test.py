"""
Check that a locally broken manifest is reported with its full error
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from shutil import rmtree


# Prepare a repo for publishing and break its manifest
init_local_crate()
# Break the manifest
with open("alire.toml", "wt") as manifest:
    manifest.write("\n---\n")

# Attempt to publish; should fail with the expected syntax error
p = run_alr("publish", force=True, complain_on_error=False)
assert_match(".*invalid syntax at.*alire\.toml.*", p.out)

# The same should happen when using the --tar option, and without backtrace
# that obscures the diagnostic, as this error is explicitly checked by Alire.
# This is a regression test for #1214.
p = run_alr("publish", "--tar", debug=False, force=True, complain_on_error=False)
assert_match("ERROR: Invalid metadata found at .*\n"
             "ERROR:    Failed to load alire.toml:\n"
             "ERROR:    Invalid TOML contents in file:\n"
             "ERROR:    invalid syntax at alire.toml:2:1\n",
             p.out)

print('SUCCESS')
