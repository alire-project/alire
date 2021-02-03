"""
Test that a dependency added as *, solved as 0.x, is narrowed down to ~0.x,
whereas one solved as 1.x is narrowed down to ^1.x
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on hello1 and hello2
run_alr('with', 'hello1', 'hello2')

# Verify solution dependencies have been properly narrowed down as ^ or ~, with
# also the proper lower version
p = run_alr('with')
assert_match('.*Dependencies \(direct\):\n'
             '   hello1~0\.1\.2\n'
             '   hello2\^1\.2\.3\n',
             p.out, flags=re.S)

print('SUCCESS')
