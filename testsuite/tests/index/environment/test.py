"""
Test proper loading of environment properties
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from glob import glob

import re
import os

# With conditionals
p = run_alr('show', 'hello')

# Test that unconditional properties are in the output
assert_match('.*'
             '   Environment: VAR1=\${VAR1}:abc\n'
             '   Environment: VAR2=xyz:\${VAR2}\n'
             '   Environment: VAR3=pqr\n'
             '   Environment: VAR4=\${_ALIRE_TEST_}\n'
             '   Environment: VAR5=\${_ALIRE_TEST_\n'
             '   Environment: VAR6=\\\\\${_ALIRE_TEST_}\n'
             '   Environment: VAR7=abc\${_ALIRE_TEST_}abc\n'
             '   Environment: VAR8=abc\\\\\${_ALIRE_TEST_}abc\n'
             '   Environment: VAR9=\${_ALIRE_TEST_}\${_ALIRE_TEST_}\${_ALIRE_TEST_}\n'
             '.*',
             p.out, flags=re.S)

# Test that conditional properties are in the output
assert_match('.*'
             '   case OS is\n'
             '      when others => Environment: CONDVAR=uvw\n'
             '.*',
             p.out, flags=re.S)

# Check resolved conditional
p = run_alr('show', 'hello', '--system')

assert_match('.*'
             '   Environment: CONDVAR=uvw\n'
             '.*',
             p.out, flags=re.S)

# Check environment variable formatting
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])
p = run_alr('printenv', '--unix')
assert_match('.*'
             'export VAR1="abc"\n'
             'export VAR2="xyz"\n'
             'export VAR3="pqr"\n'
             'export VAR4="TEST"\n'
             'export VAR5="\${_ALIRE_TEST_"\n'
             'export VAR6="\\\\\${_ALIRE_TEST_}"\n'
             'export VAR7="abcTESTabc"\n'
             'export VAR8="abc\\\\\${_ALIRE_TEST_}abc"\n'
             'export VAR9="TESTTESTTEST"\n'
             '.*',
             p.out, flags=re.S)

print('SUCCESS')
