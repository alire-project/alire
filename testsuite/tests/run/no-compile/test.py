"""
Check that running "alr run -s" fails when the project was not compiled.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Create a test project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Run it without compiling it first
p = run_alr('run', '-s', complain_on_error=False)
assert p.status != 0
assert_eq('ERROR: Executable "xxx" not found\n'
          'ERROR: alr run unsuccessful\n', 
          p.out)

print('SUCCESS')
