"""
A build waits for another alr process working on a shared build folder it has
to compile, but not once that folder has been built, nor with locking disabled
"""

import os
import subprocess

from drivers import builds, locks
from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_not_substring


def built_flag() -> str:
    return os.path.join(builds.find_dir("libhello"), "alire", "flags", "built")


def build_without_waiting(env=None):
    """
    Build while the lock is held by "another process": it must not wait
    """
    p = subprocess.run([os.environ["ALR_PATH"], "-n", "build"],
                       capture_output=True, text=True, timeout=120, env=env)
    assert_eq(0, p.returncode, p.stdout + p.stderr)
    assert_not_substring(locks.WAITING, p.stdout + p.stderr)


# A workspace with a shared dependency, synced but not compiled yet

init_local_crate()
alr_with("libhello")
builds.sync()
assert not os.path.exists(built_flag()), "Unexpectedly built"

# While another process works on its folder, the build waits to compile it

with locks.Held(locks.build_lock("libhello")):
    p = locks.Waiting(["build"],
                      message=f"{locks.WAITING} building libhello")

p.finish()
assert os.path.isfile(built_flag()), "Not marked as built"

with locks.Held(locks.build_lock("libhello")):

    # Once built, its folder no longer changes, so there is no need to wait

    build_without_waiting()

    # Nor for an alr started by the process holding the lock (e.g. from an
    # action), which it would otherwise wait for forever. The holder tells its
    # children which locks it holds.

    os.remove(built_flag())
    held = os.path.join("builds", os.path.basename(locks.build_lock("libhello")))
    build_without_waiting(env={**os.environ, "ALIRE_BUILD_LOCKS": held})

    # Nor with locking disabled, even when not built

    os.remove(built_flag())
    run_alr("settings", "--global", "--set", "dependencies.shared_locking",
            "false")
    build_without_waiting()

print("SUCCESS")
