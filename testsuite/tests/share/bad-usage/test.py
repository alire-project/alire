"""
Bad combos of `alr share` switches that should fail.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Try to set local sharing outside workspace

p = run_alr("share", "asdf", complain_on_error=False)
assert_match (".*Cannot continue without a workspace.*", p.out)

# Mix conflicting switches

p = run_alr("share", "--global", "--local", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--yes", "--no", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--yes", "--reset", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--no", "--reset", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

# Switches that require crate name

p = run_alr("share", "--local", complain_on_error=False)
assert_match (".*at least one crate.*", p.out)

p = run_alr("share", "--global", complain_on_error=False)
assert_match (".*at least one crate.*", p.out)

p = run_alr("share", "--yes", complain_on_error=False)
assert_match (".*at least one crate.*", p.out)

p = run_alr("share", "--reset", complain_on_error=False)
assert_match (".*at least one crate.*", p.out)

p = run_alr("share", "--no", complain_on_error=False)
assert_match (".*at least one crate.*", p.out)

# List incompatibilities

p = run_alr("share", "--list", "asdf", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--list", "--local", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--list", "--global", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--list", "--yes", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--list", "--no", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

p = run_alr("share", "--list", "--reset", complain_on_error=False)
assert_match (".*incompatible.*", p.out)

print('SUCCESS')
