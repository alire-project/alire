"""
Test the 'directory' property of actions
"""

import os

from drivers.alr import run_alr, init_local_crate, add_action

init_local_crate()

# Add an action that touchs file in subdir
add_action("post-fetch", ["touch", "touched"], directory="sub")

# It must initially fail as the subdir doesn't exit
p = run_alr("action", "post-fetch", complain_on_error=False)
assert p.status != 0, "Expected command to fail but didn't"

os.mkdir("sub")

# It must not fail now, and the touched file must exist
run_alr("action", "post-fetch")
assert os.path.isfile(os.path.join("sub", "touched")), "Expected file missing"

print('SUCCESS')
