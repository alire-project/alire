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
assert_match(".*unknown VCS URL", p.out)

# Bad combo, implicit file + commit
p = run_alr("publish", "fake.zip", "deadbeef", complain_on_error=False)
assert_match(".*unknown VCS URL.*", p.out)

# Missing commit for git remotes
p = run_alr("publish", "git+http://github.com/repo",
            complain_on_error=False)
assert_match(".*commit id is mandatory for a VCS origin.*", p.out)

# Detect github case and give more precise error. This serves also to check
# that github remotes without leading git+ or trailing .git are accepted.
p = run_alr("publish", "https://github.com/missingext",
            complain_on_error=False)
assert_match(".*commit id is mandatory for a VCS origin.*", p.out)

# Bad commit length
p = run_alr("publish", "git+http://github.com/repo", "deadbeef",
            complain_on_error=False)
assert_match(".*invalid git commit id, 40 digits hexadecimal expected.*",
             p.out)

# Bad commit characters
p = run_alr("publish", "git+http://github.com/repo", "_"*40,
            complain_on_error=False)
assert_match(".*invalid git commit id, 40 digits hexadecimal expected.*",
             p.out)
p = run_alr("publish", "hg+http://host.name/repo", "_"*40,
            complain_on_error=False)
assert_match(".*invalid mercurial commit id, 40 digits hexadecimal expected.*",
             p.out)

# VCS without transport or extension
p = run_alr("publish", "http://somehost.com/badrepo", "deadbeef",
            complain_on_error=False)
assert_match(".*ambiguous VCS URL.*", p.out)

print('SUCCESS')
