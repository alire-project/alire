"""
Perform a `self-update` to latest (do NOT replace the current binary)
"""

from drivers.alr import run_alr
from drivers.helpers import exe_name, MockCommand, run
import os
import sys

v_init = run_alr("version").out

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

with MockCommand("curl", curl_script, "curl_override"):
    p = run(
        [os.environ["ALR_PATH"], "-q", "-d", "-n", "self-update",
         "--location=."],
        capture_output=True,
        text=True,
    )
    out = p.stdout + p.stderr
    if p.returncode == 0:
        assert os.path.exists(exe_name("alr"))
    # Shared CI runner IPs can exhaust the live API's anonymous quota. Skip
    # only its explicit rate-limit diagnostic so other failures stay visible.
    elif "GitHub API rate limit exceeded" in out:
        print("SKIP: GitHub API rate limit exceeded")
        sys.exit()
    else:
        assert "could not find artifact" in out

assert run_alr("version").out == v_init  # ensure the main alr is unchanged

print("SUCCESS")
