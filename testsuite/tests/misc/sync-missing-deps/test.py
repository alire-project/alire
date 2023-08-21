"""
Verify that missing dependency sources are retrieved
"""

import os.path
from shutil import rmtree

from drivers.alr import run_alr
from drivers.builds import find_hash

# Create a new project and set up dependencies
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')
run_alr('with', 'hello')

target = 'alire/cache/dependencies/hello_1.0.1_filesystem'

# Ensure the hello dependency is there:
assert os.path.isdir(target), "Directory missing at expected location"

# Run commands that require a valid session after deleting a dependency. All
# should succeed and recreate the missing dependency folder.
# The first round uses sandboxed dependencies. The second round uses shared ones.
for round in range(2):
    if round == 2:
        # Prepare same test for shared dependencies
        run_alr("config", "--set", "--global", "dependencies.shared", "true")
        run_alr("update")
        target = f"builds.path()/hello_1.0.1_filesystem_{find_hash('hello')}"

    for cmd in ['build', 'pin', 'run', 'show', 'with', 'printenv']:
        # Delete folder
        rmtree(target)

        # Run the command
        run_alr(cmd)

        # The successful run should be proof enough, but check folder is there:
        assert os.path.isdir(target), \
            f"Directory missing at expected location after running command: {cmd}"


print('SUCCESS')
