"""
Ensure that changing the bounds of a configuration variable regenerates the
configuration files, even when the value itself is untouched
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import content_of

import os

VARS = """
[configuration.variables]
Var4 = {type = "Integer", first = {first}, last = {last}, default = 0}
Var6 = {type = "Real", first = {rfirst}, last = {rlast}, default = 0.0}
"""

init_local_crate()

# Keep the pristine manifest around, as TOML does not allow redefining a table
with open("alire.toml") as f:
    base_manifest = f.read()


def set_bounds(first, last, rfirst, rlast):
    """Rewrite the manifest with new bounds, leaving the values alone"""
    with open("alire.toml", "w") as f:
        f.write(base_manifest)
        f.write(VARS.format(first=first, last=last, rfirst=rfirst, rlast=rlast))


def config_file():
    for name in os.listdir("config"):
        if name.endswith("_config.ads"):
            return os.path.join("config", name)
    raise AssertionError("No generated configuration file found")


# Declare the variables and build, so the configuration is generated
set_bounds(0, 1024, 0.0, 1024.0)
run_alr("build")

ads = content_of(config_file())
assert_substring("Var4_First : constant := 0;", ads)
assert_substring("Var4_Last : constant := 1024;", ads)
assert_substring("Var6_First : constant := 0.00000000000000E+00;", ads)
assert_substring("Var6_Last : constant := 1.02400000000000E+03;", ads)

# Now change only the bounds, keeping the values. The generated configuration
# must be updated, as the bounds are part of the generated declarations.
set_bounds(10, 2048, 10.0, 2048.0)
run_alr("build")

ads = content_of(config_file())
assert_substring("Var4_First : constant := 10;", ads)
assert_substring("Var4_Last : constant := 2048;", ads)
assert_substring("Var6_First : constant := 1.00000000000000E+01;", ads)
assert_substring("Var6_Last : constant := 2.04800000000000E+03;", ads)

# The value itself is still the default one, untouched by the bounds change
assert_substring("Var4 : constant := 0;", ads)
assert_substring("Var6 : constant := 0.00000000000000E+00;", ads)

print('SUCCESS')
