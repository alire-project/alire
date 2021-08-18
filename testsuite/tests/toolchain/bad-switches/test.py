"""
Check that bad combos of switches for toolchain are detected
"""

from drivers.alr import run_alr
# from drivers.asserts import assert_eq, assert_match

p = run_alr("toolchain", "--install", "--uninstall", complain_on_error=False)
assert p.status != 0, "Call should have failed"

p = run_alr("toolchain", "--install", "--select", complain_on_error=False)
assert p.status != 0, "Call should have failed"

p = run_alr("toolchain", "--select", "--uninstall", complain_on_error=False)
assert p.status != 0, "Call should have failed"

# Bonus: test a proper invocation
p = run_alr("toolchain")

print('SUCCESS')
