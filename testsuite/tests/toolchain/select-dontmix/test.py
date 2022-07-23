"""
Check that mixing of toolchain tools is properly detected/handled
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
import re

# Capture version of "external" gnat/gprbuild
p = run_alr("toolchain")
ver = re.search("gnat_external ([0-9.]+)", p.out, re.MULTILINE).group(1)

# First, see that trying to use a native with an external is reported
p = run_alr("toolchain", "--select", "gnat_external", "gprbuild=8888",
            complain_on_error=False)
assert p.status != 0, "Expected error didn't happend"
assert_match(".*Use --force to override compatibility checks.*", p.out)

# Same thing works if forced
run_alr("toolchain", "--select", "gnat_external", "gprbuild=8888", force=True)

# Now, not forcing the native gprbuild also succeeds by automatically selecting
# gprbuild as external too
run_alr("toolchain", "--select", "gnat_external", "gprbuild")
p = run_alr("toolchain")
assert_match(f".*gprbuild     .*{ver}.*Default.*", p.out)  # External version
assert_match(f".*gnat_external.*{ver}.*Default.*", p.out)

# Likewise, picking first a native and requesting a second external tool fails
p = run_alr("toolchain", "--select", "gnat_native", "gprbuild/=8888",
            complain_on_error=False)
assert p.status != 0, "Expected error didn't happend"
assert_match(".*Use --force to override compatibility checks.*", p.out)

# But leaving free choice of gprbuild will result in the native being chosen
run_alr("toolchain", "--select", "gnat_native", "gprbuild")
p = run_alr("toolchain")
assert_match(".*gprbuild      8888.0.0 Default.*", p.out)
assert_match(".*gnat_native   8888.0.0 Default.*", p.out)
assert_match(".*gnat_external [\d\s.]+ Available.*", p.out)
# The external compiler must be still detected as available, even if unselected

# Now, trying to only select an external gnat fails because of native gprbuild
p = run_alr("toolchain", "--select", "gnat_external", complain_on_error=False)
assert p.status != 0, "Expected error didn't happend"
assert_match(".*Use --force to override compatibility checks.*", p.out)

# This can be forced again, and then we can ask for a gprbuild and again the
# proper one will be picked up automatically
run_alr("toolchain", "--select", "gnat_external", force=True)
run_alr("toolchain", "--select", "gprbuild")
p = run_alr("toolchain")
assert_match(f".*gprbuild     .*{ver}.*Default.*", p.out)
assert_match(f".*gnat_external.*{ver}.*Default.*", p.out)

# Reverse order combo also works: here we ask first for gprbuild, which
# will be solved as the latest one (native), and the appropriate gnat will be
# chosen. As we are changing both in one go, the previously selected ones do
# not restrict the new choices.
run_alr("toolchain", "--select", "gprbuild", "gnat")
p = run_alr("toolchain")
assert_match(".*gprbuild      8888.0.0 Default.*", p.out)
assert_match(".*gnat_native   8888.0.0 Default.*", p.out)
assert_match(".*gnat_external [\d\s.]+ Available.*", p.out)
# The external compiler must be still detected as available, even if unselected

# Finally, note that installing without selecting is not affected by checks,
# as these are not setting the default toolchain to be used, but merely making
# available these tools as alternatives.
run_alr("toolchain", "--install", "gnat_external", "gprbuild=8888")

print('SUCCESS')
