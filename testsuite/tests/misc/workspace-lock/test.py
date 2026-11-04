"""
A second alr on the same workspace waits for the first one, but not if the first
one started it
"""

import os
import subprocess
import threading

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_not_substring, assert_substring
from drivers.helpers import on_windows

# For manual locking simulation
if on_windows():
    import msvcrt
else:
    import fcntl

init_local_crate()

# Any command that needs the workspace creates the lock file

run_alr("show")
lock = open(os.path.join("alire", "workspace.lock"), "r+b")

# Hold the lock as another alr would. Windows locks are on the first byte.

if on_windows():
    msvcrt.locking(lock.fileno(), msvcrt.LK_NBLCK, 1)
else:
    fcntl.flock(lock, fcntl.LOCK_EX)

p = subprocess.Popen([os.environ["ALR_PATH"], "-n", "show"],
                     stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                     text=True)

# Collect stderr as it comes, to know when alr is waiting

err_lines = []
waiting = threading.Event()


def read_stderr():
    for line in p.stderr:
        err_lines.append(line)
        if "Waiting for another alr process" in line:
            waiting.set()


reader = threading.Thread(target=read_stderr)
reader.start()

assert waiting.wait(timeout=60), \
    "alr did not report waiting: " + "".join(err_lines)
assert p.poll() is None, "alr did not wait for the lock"

# Once released, it goes on

if on_windows():
    lock.seek(0)
    msvcrt.locking(lock.fileno(), msvcrt.LK_UNLCK, 1)
else:
    fcntl.flock(lock, fcntl.LOCK_UN)
lock.close()

out = p.stdout.read()
assert_eq(0, p.wait(timeout=60))
reader.join()
assert_substring("xxx=0.1.0", out)
assert_not_substring("Waiting", out)  # Messages stay out of stdout

# An action running alr on the same workspace doesn't wait for its parent

with open("alire.toml", "a") as manifest:
    manifest.write(f"""
[[actions]]
type = "post-fetch"
command = ['{os.environ["ALR_PATH"]}', "-n", "show"]
""")

p = run_alr("action", "post-fetch", quiet=False)
assert_substring("xxx=0.1.0", p.out)
assert_not_substring("Waiting", p.out)

print("SUCCESS")
