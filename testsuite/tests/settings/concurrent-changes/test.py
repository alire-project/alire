"""
Settings changed by several alr processes at the same time are all kept
"""

import os
import subprocess

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_substring

COUNT = 20

# Each process reads the settings, adds its own key and saves them. Without
# exclusion, a process may save over the change of another one. This is
# obviously not a guarantee of absence of error, but without the locking
# this test fails pretty frequently.

procs = [subprocess.Popen([os.environ["ALR_PATH"], "-n", "settings",
                           "--global", "--set", f"concurrent.key{i}", "true"])
         for i in range(COUNT)]

for p in procs:
    assert_eq(0, p.wait(timeout=120))

p = run_alr("settings", "--global", "--list")
for i in range(COUNT):
    assert_substring(f"concurrent.key{i}=true", p.out)

print("SUCCESS")
