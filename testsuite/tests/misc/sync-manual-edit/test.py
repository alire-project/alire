"""
Verify that manual changes to the manifest result in an automatic update before
other commands that require a valid workspace
"""

import os.path

from drivers import builds
from drivers.alr import run_alr, alr_touch_manifest
from shutil import rmtree
# from drivers.asserts import assert_eq, assert_match


target = 'alire/cache/dependencies/libhello_1.0.0_filesystem'

# After manually adding a dependency run commands that require a valid session.
# This should cause the expected dependency folder to exist
for cmd in ['build', 'pin', 'run', 'show', 'with', 'printenv']:

    # Create a new project
    run_alr('init', '--bin', 'xxx')
    os.chdir('xxx')

    # "Manually" add a dependency
    with open("alire.toml", "a") as file:
        file.write('[[depends-on]]\nlibhello="1"')

    # Make the lockfile "older" (otherwise timestamp is identical)
    alr_touch_manifest()

    # Run the command
    run_alr(cmd)

    # Check dependency folder is at the expected location:
    if builds.are_shared():
        assert builds.find_dir("libhello")
    else:
        assert os.path.isdir(target), "Directory missing at expected location"

    # Go back up and clean up for next command
    os.chdir("..")
    rmtree("xxx")


print('SUCCESS')
