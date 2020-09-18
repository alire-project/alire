"""
Verify that missing dependency sources are retrieved
"""

import os.path

from drivers.alr import run_alr
from shutil import rmtree
# from drivers.asserts import assert_eq, assert_match


# Create a new project and set up dependencies
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'hello')

target = 'alire/cache/dependencies/hello_1.0.1_filesystem'

# Ensure the hello dependency is there:
assert os.path.isdir(target), "Directory missing at expected location"

# Run commands that require a valid session after deleting a dependency. All
# should succeed and recreate the missing dependency folder.
for cmd in ['build', 'pin', 'run', 'show', 'with', 'printenv']:
    # Delete folder
    rmtree(target)

    # Run the command
    run_alr(cmd)

    # The successful run should be proof enough, but check folder is there:
    assert os.path.isdir(target), "Directory missing at expected location"


print('SUCCESS')
