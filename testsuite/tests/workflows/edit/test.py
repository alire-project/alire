"""
Test a basic auto withing of gpr file.
"""

from glob import glob
import os
from drivers.alr import run_alr
from drivers.asserts import assert_match

# Get the "libhello" project and enter its directory
run_alr('get', 'libhello')
os.chdir(glob('libhello*')[0])

# Check that we get an error when multiple project files are defined
p = run_alr('edit', complain_on_error=False)
assert_match(".*Please specify a project file with --project=.*", p.out)

print('SUCCESS')
