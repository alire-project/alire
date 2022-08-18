"""
Check summary of changes shown to the user when modifying dependencies
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import path_separator, with_project

# Initialize a workspace for the test
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

###############################################################################
# Add a regular solvable dependency
p = run_alr('with', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + libhello ^2.0.0 (add)

Changes to dependency solution:

   + libhello 2.0.0 (new)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Check adding a missing crate
p = run_alr('with', 'unobtanium', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + unobtanium * (add)

Changes to dependency solution:

   New solution is incomplete.
   +! unobtanium * (new,missing:unknown)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Check adding a pinned dir (the dir must exist)
os.mkdir("local_crate")
p = run_alr('with', 'local_crate', '--use=local_crate', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + local_crate * (add)

Changes to dependency solution:

   +· local_crate unknown (new,path=local_crate)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Pinning a crate to a version
p = run_alr('pin', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Changes to dependency solution:

   · libhello 2.0.0 (pin=2.0.0)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Unpinning
p = run_alr('pin', '--unpin', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Changes to dependency solution:

   o libhello 2.0.0 (unpinned)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Removal
p = run_alr('with', '--del', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   - libhello ^2.0.0 (remove)

Changes to dependency solution:

   - libhello 2.0.0 (removed)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Indirect dependency
p = run_alr('with', 'hello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + hello ^1.0.1 (add)

Changes to dependency solution:

   + hello    1.0.1 (new)""") + "\s*\n\s*" +
             re.escape("+ libhello 1.1.0 (new,indirect)") + ".*",
             p.out, flags=re.S)

###############################################################################
# Going from indirect to direct. Since the dependency is already in the
# solution, there is no version change (=).
p = run_alr('with', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + libhello ^1.1.0 (add)

Changes to dependency solution:

   = libhello 1.1.0 (direct)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Going from direct to indirect. Likewise, removing the dependency leaves it in
# the solution because is still needed indirectly via 'hello'
p = run_alr('with', '--del', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   - libhello ^1.1.0 (remove)

Changes to dependency solution:

   = libhello 1.1.0 (indirect)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Display of downgrades. Forcing libhello=1, it results in a downgrade.
p = run_alr('with', 'libhello=1', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   + libhello =1 (add)

Changes to dependency solution:

   v libhello 1.0.0 (direct,downgraded from 1.1.0)""") + ".*",
             p.out, flags=re.S)

###############################################################################
# Display of upgrades. By removing libhello=1, it can be upgraded.
p = run_alr('with', '--del', 'libhello', quiet=False)
assert_match(".*" +
             re.escape("""Requested changes:

   - libhello =1 (remove)

Changes to dependency solution:

   ^ libhello 1.1.0 (indirect,upgraded from 1.0.0)""") + ".*",
             p.out, flags=re.S)


print('SUCCESS')
