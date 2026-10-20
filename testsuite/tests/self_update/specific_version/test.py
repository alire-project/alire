"""
Perfom a self-update on a copy of the current executable
"""

import drivers.alr
from drivers.helpers import exe_name, run, shutil
from drivers.asserts import assert_substring
import time
import os

v_init = drivers.alr.run_alr("version").out

shutil.copy(os.environ["ALR_PATH"], ".")
local_alr = f".{os.sep}{exe_name('alr')}"

# alr authenticates its GitHub API calls with GH_TOKEN, while CI provides
# GITHUB_TOKEN. Unauthenticated calls are rate-limited per IP address, which
# makes this test flaky on shared CI runners.
if "GITHUB_TOKEN" in os.environ:
    os.environ.setdefault("GH_TOKEN", os.environ["GITHUB_TOKEN"])


def run_alr(args: list[str], expect_success: bool = True) -> str:
    p = run([local_alr, "-n", *args], capture_output=True)
    assert expect_success == (
        p.returncode == 0
    ), f"""stdout: {p.stdout.decode(errors="replace")}
stderr: {p.stderr.decode(errors="replace")}"""
    return p.stdout.decode(errors="replace")


initial_mtime = os.path.getmtime(local_alr)
out = run_alr(["self-update", "--release=2.1.0"])

if (
    "alr-2.1.0-bin-aarch64-linux.zip" in out
    and __import__("platform").freedesktop_os_release().get("VERSION_ID") == "22.04"
):  # HACK: no working alr 2.1.0 for ubuntu 22.04 ARM
    print("SUCCESS")
    __import__("sys").exit()

for _ in range(10):
    time.sleep(1)
    # on windows, the self-update process runs detached,
    # so we run this in a loop until it's done (or failed)
    if os.path.getmtime(local_alr) != initial_mtime:
        # detect changes by getting the file's modification time
        time.sleep(0.25)  # ongoing copy?
        break

assert_substring("2.1", run_alr(["--version"]))

assert drivers.alr.run_alr("version").out == v_init  # ensure the main alr is unchanged

print("SUCCESS")
