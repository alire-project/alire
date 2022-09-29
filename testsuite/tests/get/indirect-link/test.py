"""
Check that an indirect dependency on a linked dir is deployed correctly
"""

import re
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

# The crux of this test is to have a dependency that in turn depends on a
# linked dir: root -> tier1 -> tier2, where tier2 is linked or missing.
# For the bug to manifest, root must already depend on tier2 when tier1 is
# added. So the test adds root -> tier2, and later root -> tier1, causing:
#    root -> tier1 -> tier2
#      └----------------^
# This indeed does not work with alr versions pre- this PR.


# Initialize root workspace
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Depend on tier2, as a linked idr
os.mkdir('tier2')
run_alr('with', 'tier2', '--use', 'tier2')

# Add tier1 (this is where the bug manifests pre- fix)
run_alr('with', 'tier1')

# Verify the solution graph looks as expected
p = run_alr('with', '--solve')
assert_match('.*' +
             re.escape('Dependencies (graph):\n'
                       '   tier1=1.0.0   --> tier2*\n'
                       '   xxx=0.1.0-dev --> tier1=1.0.0 (^1.0.0)\n'
                       '   xxx=0.1.0-dev --> tier2^1.0.0\n') + '.*',
             p.out, flags=re.S)


print('SUCCESS')
