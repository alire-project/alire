"""
Check `alr index-test` of the local release
"""

import os

from drivers.alr import add_action, init_local_crate, run_alr
from drivers.asserts import assert_file_exists, assert_in_file

# Create a crate with a local release
init_local_crate()
run_alr("index-test") # Ending with success is enough

# Check the expected log files exist
assert_file_exists(os.path.join("alire", "alr_test_local.log"))
assert_file_exists(os.path.join("alire", "alr_test_local.xml"))

# Check the build is performed in release mode
assert_in_file(os.path.join("config", "xxx_config.gpr"),
               'Build_Profile : Build_Profile_Kind := "release";')

# Check testing from a subdirectory in a new crate
os.chdir("..")
init_local_crate("yyy")
os.chdir("src")
run_alr("index-test") # Ending with success is enough

# Check the expected log files exist
assert_file_exists(os.path.join("..", "alire", "alr_test_local.log"))
assert_file_exists(os.path.join("..", "alire", "alr_test_local.xml"))

# Check testing with a test action instead of default build
os.chdir("..")
init_local_crate("zzz")
add_action("test", ["touch", "success.txt"])
run_alr("index-test")
assert_file_exists("success.txt")

# Likewise from a subdirectory
os.remove("success.txt")
os.chdir("src")
run_alr("index-test")
assert_file_exists(os.path.join("..", "success.txt"))


print('SUCCESS')
