"""
Tests that missing mandatory/recommended properties are reported during publish
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import init_git_repo

# We have two tarballs ready in the test local index. One is missing maintainer
# and the other is missing optional tags.

# Attempt with crate missing maintainer
p = run_alr("publish", "nomaint.tgz", "--skip-submit",
            complain_on_error=False, force=True)
assert_match(".*Missing required properties: maintainers.*", p.out)

# Attempt with crate missing optional recommended properties. No quiet or the
# warning on optional properties is silenced.
p = run_alr("publish", "notags.tgz", "--skip-submit",
            quiet=False, force=True)
assert_match(".*Missing optional recommended properties:"
             " authors, licenses, tags, website.*",
             p.out)

print('SUCCESS')
