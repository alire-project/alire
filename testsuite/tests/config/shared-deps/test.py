"""
Check that globally sharing dependencies works as expected
"""

import os
from drivers.alr import alr_with, alr_workspace_cache, init_local_crate, run_alr
from drivers.asserts import assert_contents, assert_file_exists
from drivers.helpers import lines_of, replace_in_file

deps_dir = "deps"

# Check that using a relative path fails, even if it exists
os.mkdir("fake")
run_alr("config", "--global", "--set", "dependencies.dir", "fake",
        complain_on_error=False)

# Check that using an absolute non-existing path fails
run_alr("config", "--global", "--set", "dependencies.dir",
        os.path.abspath(deps_dir),
        complain_on_error=False)

# Succeed after creating the destination
os.mkdir(deps_dir)
run_alr("config", "--global", "--set", "dependencies.dir",
        os.path.abspath(deps_dir))

# Create a crate with a dependency
init_local_crate()
alr_with("hello")

# Ensure the dependencies are where expected
assert_file_exists(os.path.join("..", deps_dir, "hello_1.0.1_filesystem"))
assert_file_exists(os.path.join("..", deps_dir, "libhello_1.0.0_filesystem"))

# Check contents of one of the dependencies to make even surer
assert_contents(os.path.join("..", deps_dir, "hello_1.0.1_filesystem"),
                ['../deps/hello_1.0.1_filesystem/alire',
                 '../deps/hello_1.0.1_filesystem/alire.toml',
                 '../deps/hello_1.0.1_filesystem/config',
                 '../deps/hello_1.0.1_filesystem/config/hello_config.ads',
                 '../deps/hello_1.0.1_filesystem/config/hello_config.gpr',
                 '../deps/hello_1.0.1_filesystem/config/hello_config.h',
                 '../deps/hello_1.0.1_filesystem/hello.gpr',
                 '../deps/hello_1.0.1_filesystem/src',
                 '../deps/hello_1.0.1_filesystem/src/hello.adb'])

# And that the crate usual cache dir doesn't exist
assert not os.path.exists(alr_workspace_cache())

# Import the dependency in our code to ensure build works with the new cache
# location
new_code = ["with Hello;\n"] + lines_of(os.path.join("src", "xxx.adb"))
with open(os.path.join("src", "xxx.adb"), "w") as f:
    f.writelines(new_code)

run_alr("build")


print('SUCCESS')
