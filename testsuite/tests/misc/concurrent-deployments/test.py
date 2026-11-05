"""
Several alr processes deploying the same release into a location shared by all
workspaces at the same time all succeed, and leave a complete deployment. We
use a toolchain, as its deployment involves nothing else shared (unlike a
dependency, whose shared build folder also gets synced).
"""

import os
import shutil
import subprocess

from drivers import builds, locks
from drivers.alr import alr_settings_dir, run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import MockCommand

COUNT = 20
# No guarantee of triggering an error, of course, but it happens almost
# always in local runs without this feature.

# A slow tar keeps all deployments in progress at the same time, so that they
# finish together, as happens with bigger releases. The release comes from a
# source archive, so extracting it takes a call to tar.

TAR = f"""\
import subprocess, sys, time
time.sleep(1)
sys.exit(subprocess.call([{shutil.which("tar")!r}] + sys.argv[1:]))
"""

with MockCommand("tar", TAR, "mocks"):
    procs = [subprocess.Popen([os.environ["ALR_PATH"], "-n", "toolchain",
                               "--select", "gnat_native=8888"],
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                              text=True)
             for _ in range(COUNT)]

    outputs = []
    for i, p in enumerate(procs):
        out, _ = p.communicate(timeout=120)
        assert_eq(0, p.returncode, f"Process {i} failed:\n{out}")
        outputs.append(out)

# Only one did it, while the others said they were waiting

assert any(f"{locks.WAITING} deploying gnat_native=8888.0.0"
           in out for out in outputs), "\n".join(outputs)

# The deployment is complete, and there is only one

toolchains = os.path.join(alr_settings_dir(), "cache", "toolchains")
deployed = [d for d in os.listdir(toolchains) if d.startswith("gnat_native")]
assert_eq(1, len(deployed))
assert os.path.isfile(builds.complete_copy_flag(
    os.path.join(toolchains, deployed[0]))), \
    "Incomplete deployment"

assert_match(".*gnat_native.*8888.0.0.*Available.*",
             run_alr("toolchain").out)

print("SUCCESS")
