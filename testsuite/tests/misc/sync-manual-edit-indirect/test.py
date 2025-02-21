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


MAIN_CRATE = "main"
DEP_CRATE  = "dep"

def set_up():
    initial_dir = os.getcwd()

    # Ensure clean state
    rmtree(MAIN_CRATE, ignore_errors=True)
    rmtree(DEP_CRATE, ignore_errors=True)

    # Indirect crate we will edit
    init_local_crate(DEP_CRATE, enter=False)

    # Main crate    
    init_local_crate(MAIN_CRATE)
    alr_with(DEP_CRATE, path=f"../{DEP_CRATE}", update=True)

    # Add new dependency to the linked crate
    os.chdir(f"../{DEP_CRATE}")
    alr_with("libhello")
    prepend_to_file(f"src/{DEP_CRATE}.adb",
                    ["with Libhello;"])
    
    # Back to the root directory
    os.chdir(initial_dir)


# After manually adding a dependency run commands that require a valid session.
# This should cause the expected dependency folder to exist
for cmd in ['build', 'pin', 'run', 'show', 'with', 'printenv']:
    
    # Prepare faulty condition
    set_up()    

    # Run the command in the main crate folder
    os.chdir(MAIN_CRATE)
    p = run_alr(cmd, quiet=False)

    # If no error was reported, then we should be okay. Still, check that the
    # update happened as expected:
    assert_substring("Changes detected in pinned dependencies", p.out)

    # Go back up
    os.chdir("..")    

print('SUCCESS')
