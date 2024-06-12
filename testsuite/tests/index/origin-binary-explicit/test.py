"""
Test that origins explicitly marked as binary are interpreted as such, even if
they do not use a dynamic expression.
"""

from glob import glob
import os
from drivers import builds
from drivers.alr import alr_settings_dir, alr_with, run_alr, init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import contents

# Check basic loading
p = run_alr('show', 'hello_world')
assert "Origin: binary archive" in p.out, \
    "Unexpected output: " + p.out

init_local_crate()
alr_with("hello_world")
run_alr("build")

# Check deployment location
if builds.are_shared():
    # Build dir of a binary crate should not exist, as it is used directly from
    # its download location
    assert len(glob(f"{builds.path()}/hello_world*")) ==0 , \
        "Unexpected build dir: " + str(contents(builds.path()))
else:
    # Config should not have been generated for a binary crate during build
    assert os.path.exists(os.path.join("alire", "cache", "dependencies",
                                       "hello_world_0.1.0_c17d6ce8"))
    assert not os.path.exists(os.path.join("alire", "cache", "dependencies",
                                           "hello_world_0.1.0_c17d6ce8",
                                           "config")), \
        "Unexpected config dir: " + str(contents("alire"))

print('SUCCESS')
