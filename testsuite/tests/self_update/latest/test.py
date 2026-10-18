"""
Perform a `self-update` to latest (do NOT replace the current binary)
"""

import os

from drivers.alr import run_alr
from drivers.helpers import exe_name, run

# alr authenticates its GitHub API calls with GH_TOKEN, while CI provides
# GITHUB_TOKEN. Unauthenticated calls are rate-limited per IP address, which
# makes this test flaky on shared CI runners.
if "GITHUB_TOKEN" in os.environ:
    os.environ.setdefault("GH_TOKEN", os.environ["GITHUB_TOKEN"])

v_init = run_alr("version").out

# The update may legitimately fail when there is no release artifact for the
# current platform, so check the outcome ourselves
p = run([os.environ["ALR_PATH"], "-n", "self-update", "--location=."],
        capture_output=True)
out = p.stdout.decode(errors="replace") + p.stderr.decode(errors="replace")

if p.returncode == 0:
    assert os.path.exists(exe_name("alr")), out
else:
    assert "could not find artifact" in out, out

assert run_alr("version").out == v_init  # ensure the main alr is unchanged

print("SUCCESS")
