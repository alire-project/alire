"""
Perfom a self-update on a copy of the current executable
"""

import drivers.alr
from drivers.helpers import exe_name, MockCommand, run, shutil
from drivers.asserts import assert_substring
import time
import os

v_init = drivers.alr.run_alr("version").out

shutil.copy(os.environ["ALR_PATH"], ".")

curl_script = """
import os
import subprocess
import sys

env2 = os.environ.copy()
env2["PATH"] = env2["PATH"].split(os.pathsep, 1)[1]
token_header = []
if "GITHUB_TOKEN" in os.environ:
    token_header = ["-H", f"Authorization: Bearer {os.environ['GITHUB_TOKEN']}"]
subprocess.call(["curl", *token_header, *sys.argv[1:]], env=env2)
"""


def run_alr(args: list[str], expect_success: bool = True) -> str:
    p = run(
        [f".{os.sep}{exe_name('alr')}", "-n", *args],
        capture_output=True,
        check=expect_success,
    )
    return str(p.stdout)


with MockCommand("curl", curl_script, "curl_override"):
    run_alr(["self-update", "--release=2.0"])

    out = None
    for i in range(10):
        out = run_alr(["--version"])
        if "2.0" in out:
            break
        time.sleep(1)

assert_substring("2.0", out)

assert drivers.alr.run_alr("version").out == v_init  # ensure the main alr is unchanged

print("SUCCESS")
