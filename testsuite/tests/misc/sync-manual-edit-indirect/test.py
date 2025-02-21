"""
Verify that manual changes to the manifest of a linked dependency result in
an automatic update before other commands that require a valid workspace. This 
was bug https://github.com/alire-project/alire/issues/1662
"""

import os.path

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import prepend_to_file
from shutil import rmtree


target = 'alire/cache/dependencies/libhello_1.0.0_filesystem'

def set_up():
    initial_dir = os.getcwd()

    # Indirect crate we will edit
    init_local_crate("dep", enter=False)

    # Main crate
    init_local_crate("main")
    alr_with("dep", path="../dep", update=True)

    # Add new dependency to the linked crate
    os.chdir("../dep")
    alr_with("libhello")
    prepend_to_file("src/dep.adb",
                    ["with Libhello;"])
    
    # Back to the root directory
    os.chdir(initial_dir)


# After manually adding a dependency run commands that require a valid session.
# This should cause the expected dependency folder to exist
for cmd in ['build', 'pin', 'run', 'show', 'with', 'printenv']:
    
    # Prepare faulty condition
    set_up()    

    # Run the command in the main crate folder
    os.chdir("main")
    p = run_alr(cmd)

    # If no error was reported, then we should be okay. Still, check that the
    # update happened as expected:
    assert_substring("asdf", p.out)

    # Go back up and clean up for next command
    os.chdir("..")
    rmtree("xxx")


print('SUCCESS')
