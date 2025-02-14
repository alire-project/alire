"""
https://github.com/alire-project/alire/issues/1806
Ensure that an error in an action is not masked by success in a following one.
"""

import os
import uuid
from drivers.alr import init_local_crate, run_alr, add_action
from drivers.asserts import assert_substring


def check_errors():
    assert_substring("failed",
                 run_alr("test", complain_on_error=False).out)
    assert_substring("action exited with error",
                    run_alr("action", "test", complain_on_error=False).out)


init_local_crate()

# Verify a successful action
add_action("test", ["echo", "OK"])
run_alr("test")

# Verify a failing action both via `alr test` and `alr action`
uuid = uuid.uuid4().hex
assert not os.path.exists(uuid)
add_action("test", ["ls", uuid])
check_errors()

# Verify that a subsequent successful action does not mask the error
add_action("test", ["echo", "OK"])
check_errors()

print("SUCCESS")
