"""
Verify that an origin definition is rejected in a local manifest
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from os.path import join


init_local_crate("xxx")

# Add manually the origin, and verify that it cannot be loaded
with open(join("alire", "xxx.toml"), "a") as file:
    file.write("\n[origin]\n")

p = run_alr("show", complain_on_error=False)
assert_match(".*invalid property: origin.*", p.out)

print('SUCCESS')
