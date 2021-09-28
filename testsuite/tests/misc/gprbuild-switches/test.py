"""
Verify alr build switches are passed to gprbuild
"""

from glob import glob
import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Switches before a double-dash are parsed for the alr build command
p = run_alr('build', '-XTEST=TEST',
            complain_on_error=False, debug=False)

assert_match('ERROR: Unrecognized option \'-X\'.*',
             p.out,
             flags=re.S)

# Switches after a double-dash are passed to gprbuild
p = run_alr('build', '--', '--this-is-not-a-valid-switch',
            complain_on_error=False, debug=False)

assert_match('.*gprbuild: illegal option'
             ' "--this-is-not-a-valid-switch"'
             ' on the command line.*',
             p.out,
             flags=re.S)


print('SUCCESS')
