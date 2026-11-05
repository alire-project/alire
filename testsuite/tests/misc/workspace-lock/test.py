"""
A second alr on the same workspace waits for the first one, but not if the first
one started it
"""

from drivers import locks
from drivers.alr import add_action, init_local_crate, run_alr
from drivers.asserts import assert_not_substring, assert_substring

init_local_crate()

# While another alr process holds the workspace, alr waits for it, and goes on
# once released

with locks.Held(locks.workspace_lock()):
    p = locks.Waiting(["show"])

assert_substring("xxx=0.1.0", p.finish())

# An action running alr on the same workspace doesn't wait for its parent

add_action("post-fetch", ["alr", "-n", "show"])

p = run_alr("action", "post-fetch", quiet=False)
assert_substring("xxx=0.1.0", p.out)
assert_not_substring(locks.WAITING, p.out)

print("SUCCESS")
