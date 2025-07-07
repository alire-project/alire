"""
Perform a `self-update` to latest (do NOT replace the current binary)
"""

from drivers.alr import run_alr
from drivers.helpers import exe_name, MockCommand
import os

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
    run_alr("self-update", "--location=.")

assert os.path.exists(exe_name("alr"))

assert run_alr("version").out == v_init  # ensure the main alr is unchanged

print("SUCCESS")
