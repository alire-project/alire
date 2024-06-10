"""
Check the several stop points in the build process
"""

from enum import IntEnum
import os
import shutil
from drivers.alr import run_alr, init_local_crate, add_action, alr_with
from drivers import builds
# from drivers.asserts import assert_eq, assert_match


class StopStage(IntEnum):
    sync       = 0
    generation = 1
    post_fetch = 2
    pre_build  = 3
    build      = 4
    post_build = 5


# A function that checks things are as they should be
def check_stop(stop: StopStage):
    # Run alr
    out = run_alr("build", f"--stop-after={stop.name.replace('_', '-')}",
                  quiet=False).out

    # Sync should have happened
    if builds.are_shared():
        assert builds.find_dir("libhello"), "libhello build dir should exist"

    # Check generation of config files
    assert (stop >= StopStage.generation) == os.path.isfile("config/xxx_config.gpr"), \
        f"config dir existence [un]expected: {idx}, {os.path.isfile('config/xxx_config.gpr')}"

    # Check post-fetch, which should happen only once
    assert (stop == StopStage.post_fetch) == ("post-fetch-triggered" in out), \
        f"post-fetch-triggered should be in output: {out}"

    # Check pre-build, which should happen in every build that goes beyond it
    assert (stop >= StopStage.pre_build) == ("pre-build-triggered" in out), \
        f"pre-build-triggered should be in output: {out}"

    # Check build artifacts exist only after build
    assert (stop >= StopStage.build) == os.path.isdir("obj"), \
        f"obj dir existence [un]expected"

    # Check post-build
    assert (stop >= StopStage.post_build) == ("post-build-triggered" in out), \
        f"post-build-triggered should be in output: {out}"


# TEST BEGIN

# Prepare the crate with actions and a dependency whose syncing we can test
init_local_crate()
add_action("post-fetch", ["echo", "post-fetch-triggered"])
add_action("pre-build",  ["echo", "pre-build-triggered"])
add_action("post-build", ["echo", "post-build-triggered"])
alr_with("libhello")

# Remove config dir which may have been generated during initialization
shutil.rmtree("config")

# Check that the stop points are honored
for stop in StopStage:
    check_stop(stop)

print("SUCCESS")
