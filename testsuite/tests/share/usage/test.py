"""
Check that selective sharing works as expected both locally and globally, and
also when global sharing is enabled through the 'dependencies.dir' config.
"""

import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import match_deploy_dir

startdir = os.getcwd()
global_cache = "alr-config"
custom_cache = "custom_deps"

def reset():
    # Reset to initial test state
    os.chdir(startdir)
    shutil.rmtree("xxx", ignore_errors=True) # May not exist
    shutil.rmtree("yyy", ignore_errors=True) # May not exist


def local_cache(crate : str = "xxx") -> str:
    """
    Return the partial path to the local cache for a crate
    """
    return os.path.join(crate, "alire", "cache", "dependencies")


# Check usual, simplest way of requesting a shared dependency
init_local_crate()
run_alr("share", "libhello")
run_alr("with", "libhello")
match_deploy_dir("libhello", global_cache)
reset()

# Check that explicit switch works the same
init_local_crate()
run_alr("share", "--yes", "libhello")
run_alr("with", "libhello")
match_deploy_dir("libhello", global_cache)
reset()

# Check global sharing hasn't occurred unexpectedly
init_local_crate()
run_alr("with", "libhello")
match_deploy_dir("libhello", local_cache())
reset()

# Check global caching now
init_local_crate()
run_alr("share", "--global", "libhello")
os.chdir(startdir)
init_local_crate("yyy")
run_alr("with", "libhello")
match_deploy_dir("libhello", global_cache)
reset()

# Check that global caching can be prevented locally
os.chdir(startdir)
init_local_crate("yyy")
run_alr("share", "--no", "libhello")
run_alr("with", "libhello")
match_deploy_dir("libhello", local_cache("yyy"))
reset()

# Check now interaction with global sharing through 'dependencies.dir'

# Enable and verify global sharing
os.chdir(startdir)
os.mkdir(custom_cache)
run_alr("config", "--global", "--set", "dependencies.dir",
        os.path.join(os.getcwd(), custom_cache))
init_local_crate()
run_alr("with", "hello")
match_deploy_dir("hello", custom_cache)
reset()

# Verify that local sharing overrides global sharing
init_local_crate()
run_alr("share", "--no", "hello")
run_alr("with", "hello")
match_deploy_dir("hello", local_cache())


print('SUCCESS')
