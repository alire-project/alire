"""
Check `alr test` of the local release using a docker image
"""

import os
import subprocess

from drivers.alr import add_action, init_local_crate, run_alr
from drivers.asserts import assert_file_exists, assert_in_file
from drivers.driver.docker_wrapper import skip_unless_docker_available, is_docker_available

skip_unless_docker_available()

# Create a crate with a local release
init_local_crate()

run_alr("test", "--docker") # Ending with success is enough
assert_in_file(os.path.join("alire", "alr_test_local.log"),
            "sudo docker run") # Ensure this was run in docker

# Check testing with a test action instead of default build
add_action("test", ["touch", "success.txt"])
run_alr("test", "--docker")
assert_file_exists("success.txt")

print('SUCCESS')
