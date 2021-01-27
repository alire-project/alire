"""
Test a basic get-build-run workflow.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Build it
run_alr('build')

# Run it
p = run_alr('run')
assert_eq('Hello, world!\n', p.out)

# Clean it
assert os.listdir('obj')
run_alr('clean')
assert not os.listdir('obj')

print('SUCCESS')
