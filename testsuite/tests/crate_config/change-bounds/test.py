"""
Ensure that changing the bounds of a configuration variable regenerates the
configuration files, even when the value itself is untouched
"""

from drivers.alr import init_local_crate, run_alr
from drivers.helpers import content_of

import os
import re

VARS = """
[configuration.variables]
Var4 = {{type = "Integer", first = {first}, last = {last}, default = 0}}
Var6 = {{type = "Real", first = {rfirst}, last = {rlast}, default = 0.0}}
"""

# Both ranges must contain the default (0), as the manifest is rejected
# otherwise, so they are widened symmetrically around zero.
FIRST, LAST = -1024, 1024
NEW_FIRST, NEW_LAST = -2048, 2048

init_local_crate()

# Keep the pristine manifest around, as TOML does not allow redefining a table
with open("alire.toml") as f:
    base_manifest = f.read()


def set_bounds(first, last):
    """Rewrite the manifest with new bounds, leaving the values alone"""
    with open("alire.toml", "w") as f:
        f.write(base_manifest)
        f.write(VARS.format(first=first, last=last,
                            rfirst=float(first), rlast=float(last)))


def config_file():
    for name in os.listdir("config"):
        if name.endswith("_config.ads"):
            return os.path.join("config", name)
    raise AssertionError("No generated configuration file found")


def assert_decl(ads, name, image):
    """
    Check a declaration in the generated config. The value is matched loosely
    on whitespace, as 'Img pads non-negative numbers with a leading space
    """
    pattern = rf"^\s*{name} : constant := +{re.escape(image)};$"
    assert re.search(pattern, ads, re.M), \
        f"Missing {name} declaration with value {image} in:\n{ads}"


# Declare the variables and build, so the configuration is generated
set_bounds(FIRST, LAST)
run_alr("build")

ads = content_of(config_file())
assert_decl(ads, "Var4_First", str(FIRST))
assert_decl(ads, "Var4_Last", str(LAST))
assert_decl(ads, "Var6_First", "-1.02400000000000E+03")
assert_decl(ads, "Var6_Last", "1.02400000000000E+03")

# Now change only the bounds, keeping the values. The generated configuration
# must be updated, as the bounds are part of the generated declarations.
set_bounds(NEW_FIRST, NEW_LAST)
run_alr("build")

ads = content_of(config_file())
assert_decl(ads, "Var4_First", str(NEW_FIRST))
assert_decl(ads, "Var4_Last", str(NEW_LAST))
assert_decl(ads, "Var6_First", "-2.04800000000000E+03")
assert_decl(ads, "Var6_Last", "2.04800000000000E+03")

# The values themselves are still the defaults, untouched by the bounds change
assert_decl(ads, "Var4", "0")
assert_decl(ads, "Var6", "0.00000000000000E+00")

print('SUCCESS')
