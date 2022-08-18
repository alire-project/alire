"""
Check output of the --tree switch
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Initialize project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Add dependency on hello^1. Solution is hello=1.0.1 --> libhello=1.1.0
run_alr('with', 'hello^1')

# Add dependency on superhello*. Solution is superhello=1.0 --> libhello=1.0.1
# This implies a downgrade from libhello=1.1.0 to libhello=1.0.1, which is the
# only possible combination of libhello^1.0 & libhello~1.0
run_alr('with', 'superhello')

# Add more dependencies, without a proper release
os.mkdir("fake")
run_alr('with', 'wip', '--use', 'fake', force=True)  # force bc dir is missing
run_alr('with', 'unobtanium', force=True)

# Verify printout (but for test-dependent path)
# Note that superhello was auto-narrowed down to ^1, but missed ones did not
p = run_alr('with', '--tree')
assert_match(re.escape('''xxx=0.1.0-dev
+-- hello=1.0.1 (^1)
|   +-- libhello=1.0.1 (^1.0)
+-- superhello=1.0.0 (^1.0.0)
|   +-- libhello=1.0.1 (~1.0)
+-- unobtanium* (direct,missed:unknown) (*)
+-- wip* (direct,linked,path=''') + '.*' + re.escape(') (*)'),
             p.out, flags=re.S)

print('SUCCESS')
