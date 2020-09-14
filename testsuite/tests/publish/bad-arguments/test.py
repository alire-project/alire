"""
Tests for bad arguments to publish
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Check that a local zip is rejected (as it must be remote)
p = run_alr("publish", "file:/fake.zip", complain_on_error=False)
assert_match(".*The origin must be a definitive remote location.*", p.out)

# Bad combo, explicit file + commit
p = run_alr("publish", "file:/fake.zip", "deadbeef", complain_on_error=False)
assert_match(".*Expected a VCS origin but got.*", p.out)

# Bad combo, implicit file + commit
p = run_alr("publish", "fake.zip", "deadbeef", complain_on_error=False)
assert_match(".*unknown VCS URL.*", p.out)

# VCS without transport or extension
p = run_alr("publish", "http://github.com/badrepo", "deadbeef",
            complain_on_error=False)
assert_match(".*ambiguous VCS URL.*", p.out)

print('SUCCESS')
