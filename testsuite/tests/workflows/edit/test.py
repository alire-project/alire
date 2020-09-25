"""
Test alr edit with two project files defined.
"""

from glob import glob
from drivers.alr import run_alr
from drivers.asserts import assert_match
import os
import shutil


# Get the "libhello" project and enter its directory
run_alr('get', 'libhello')
os.chdir(glob('libhello*')[0])

gs = shutil.which('gnatstudio')

if gs is None:
    # GNATstudio not in PATH: Check that we get an error saying GS not
    # available
    p = run_alr('edit', complain_on_error=False)
    assert_match(".*GNATstudio not available or not in PATH.*", p.out)
else:
    # GNATstudio in PATH: Check that we get an error when multiple project
    # files are defined
    p = run_alr('edit', complain_on_error=False)
    assert_match(".*Please specify a project file with --project=.*", p.out)

print('SUCCESS')
