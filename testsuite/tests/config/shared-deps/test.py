"""
Check that globally sharing builds works as expected
"""

import glob
import os

from drivers import builds
from drivers.alr import (alr_builds_dir, alr_vault_dir, alr_with,
                         alr_workspace_cache, init_local_crate, run_alr)
from drivers.asserts import assert_contents, assert_file_exists
from drivers.helpers import contents, lines_of, neutral_path


def check_in(file : str, expected : str) -> bool:
    assert file in expected, f"Missing file '{file}' in\n{expected}"


vault_dir = alr_vault_dir()
build_dir = alr_builds_dir()

# Create a crate with a dependency
init_local_crate()
alr_with("hello")

# Ensure the "read-only" sources are where expected
assert_file_exists(os.path.join(vault_dir, "hello_1.0.1_filesystem"))
assert_file_exists(os.path.join(vault_dir, "libhello_1.0.0_filesystem"))

# Check contents of one of the dependencies to make even surer
assert_contents(base := os.path.join(vault_dir, "hello_1.0.1_filesystem"),
                [f'{base}/alire',
                 f'{base}/alire.toml',
                 f'{base}/alire/flags',
                 f'{base}/alire/flags/complete_copy',
                 f'{base}/hello.gpr',
                 f'{base}/src',
                 f'{base}/src/hello.adb'])

# Check the contents in the build dir, that should not include generated config
# because no build has been attempted yet, hence a sync has not been performed.
# And, since there's no sync yet, neither the build dir exists:

assert len(glob.glob(os.path.join(build_dir, "hello_1.0.1_filesystem_*"))) == 0, \
    "Build dir should not exist yet"

# Do a build, and now the sync should have happened and the build dir be filled
run_alr("build")
base = builds.find_dir("hello_1.0.1_filesystem")

# There's too much object files and the like, check a few critical files:
files = contents(base)  # This returns "normalized" paths (with '/' separators)
nbase = neutral_path(base)
check_in(f'{nbase}/config/hello_config.ads', files)     # config was generated
check_in(f'{nbase}/alire/flags/post_fetch_done', files) # actions were run
# check_in(f'{nbase}/obj/b__hello.ads', files)          # build took place
# The build actually doesn't take place because the dependency is not used.
# Due to a former bug, where all deps were built, the previous check succeeded.
# The line is left in case this bug reappears, so it's easier to re-understand.

# And that the crate usual cache dir doesn't exist
assert not os.path.exists(alr_workspace_cache())

# Import the dependency in our code to ensure build works with the new cache
# location
new_code = ["with Hello;\n"] + lines_of(os.path.join("src", "xxx.adb"))
with open(os.path.join("src", "xxx.adb"), "w") as f:
    f.writelines(new_code)

run_alr("build")


print('SUCCESS')
