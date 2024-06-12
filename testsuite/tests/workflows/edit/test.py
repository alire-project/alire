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

# Set GNATstudio as prefered editor
run_alr('settings', '--set', 'editor.cmd', 'gnatstudio -P ${GPR_FILE}')

gs = shutil.which('gnatstudio')

if gs is None:
    # GNATstudio not in PATH: Check that we get an error saying GS not
    # available
    p = run_alr('edit', complain_on_error=False)
    assert_match(".*GNAT Studio not available or not in PATH.*", p.out)
else:
    # GNATstudio in PATH: Check that we get an error when multiple project
    # files are defined
    p = run_alr('edit', complain_on_error=False)
    assert_match(".*Please specify a project file with --project=.*", p.out)

# Set an editor that doesn't exist (different than GNAT Studio)
run_alr('settings', '--set', 'editor.cmd', 'doesnt_exist arg1 ab${GPR_FILE}ab arg3')
p = run_alr('edit', '--project=project1.gpr', complain_on_error=False)
assert_match("ERROR: 'doesnt_exist' not available or not in PATH.*", p.out)
print(p.out)

# Use echo as an editor to check command line args
run_alr('settings', '--set', 'editor.cmd', 'echo arg1 ab${GPR_FILE}ab arg3')
p = run_alr('edit', '--project=project1.gpr')
assert_match("arg1 abproject1.gprab arg3", p.out)

print('SUCCESS')
