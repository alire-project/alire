"""
Check output of with --versions switch
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

# Add a pinned directory and a missing dependency
run_alr('with', 'wip', '--use', '/fake')
run_alr('with', 'unobtanium', '--force')

# Check output
p = run_alr('with', '--versions')

assert_match
(re.escape
 ('CRATE      DEPENDENCY      SOLVED  LATEST \n'
  'hello      ^1              1.0.1   4.0.0  \n'
  'libhello   (^1.0) & (~1.0) 1.0.1   2.0.0  \n'
  'superhello *               1.0.0   1.0.0  \n'
  'unobtanium *               missing unknown\n'
  'wip        *               ') + '.*fake' + re.escape('unknown\n'
  'xxx        (root)          0.0.0   unknown\n'),
  p.out)

print('SUCCESS')
