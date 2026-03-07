"""
Pin forcibly a dependency that cause missing dependencies
"""

import re
import os

from drivers.alr import run_alr, alr_pin
from drivers.asserts import assert_match
from glob import glob


# Initialize a new crate and add the "hello*" dependency. This is solved as:
#    xxx=0.0.0 -> hello=1.0.1 --> libhello=1.1.0
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'hello>0')

# 1st test: pin to an existing version that brings in missing dependencies.
# Pinning hello=3 brings in a libhello^3 dependency that is unavailable, so:
#    xxx=0.0.0 -> hello=3.0.0 --> libhello^3 (missing)
alr_pin('hello', version='3')

# Check solution is as expected
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(solution\):\n'
             '   hello=3\.0\.0 \(pinned\).*\n'  # skip irrelevant origin info
             '.*Dependencies \(missing\):\n'
             '   libhello\^3\.0.*',
             p.out, flags=re.S)

# 2nd test: directly pin to a missing version (hello=5). This causes  libhello
# to disappear from the solution, since hello's dependencies are now unknown:
#    xxx=0.0.0 -> hello=5 (missing)
alr_pin('hello', version='5')

# Check solution is as expected. Depending on the solver options and
# optimizations, this may show as hello=5.0.0 or hello(=5.0.0&>0), hence the
# permissive regex
p = run_alr('with', '--solve')
assert_match('.*Dependencies \(missing\):\n'
             '   hello.*=5\.0\.0.*',
             p.out, flags=re.S)


print('SUCCESS')
