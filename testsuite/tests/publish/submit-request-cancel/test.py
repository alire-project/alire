"""
A complete online test of submitting a release, requesting a review, and
cancelling the request.
"""

import os
import random
import re
import subprocess
import sys
import time
from drivers.alr import run_alr
from drivers.helpers import git_init_user

# This test is a bit special, as it is heavy on networking and complex on
# automation. Not intended to be run on CI, but rather on the developer's local
# machine. We also require having the `gh` command installed and configured,
# GH_TOKEN set to a valid token, and GH_USERNAME set to a valid username.

# IMPORTANT: if the test fails midway with the PR open, you must manually close
# the PR at https://github.com/alire-project/test-index/pulls

# Detect gh via gh --version
if subprocess.run(["gh", "--version"]).returncode != 0:
    print("SKIP: `gh` not installed")
    sys.exit(0)

# Detect GH_TOKEN
if os.environ.get("GH_TOKEN", "") == "":
    print("SKIP: GH_TOKEN not set")
    sys.exit(0)

# Detect GH_USERNAME
if os.environ.get("GH_USERNAME", "") == "":
    print("SKIP: GH_USERNAME not set")
    sys.exit(0)
else:
    run_alr("config", "--global", "--set",
            "user.github_login", os.environ["GH_USERNAME"])

# Configure the testing remote index
run_alr("config", "--global", "--set", "index.repository_name", "test-index")

# Clone a simple crate not already in the index with a local remote
subprocess.run(["gh", "repo", "clone",
                "alire-project/hello", "hello_upstream",
                "--", "--bare"]).check_returncode()
subprocess.run(["git", "clone", "hello_upstream", "hello"]).check_returncode()
os.chdir("hello")

# To allow eventual concurrent testing, modify the crate version to a random
# one. We need to replace the version in alire.toml only. To avoid troubles
# with line endings, we edit in binary mode.
with open("alire.toml", "rb") as f:
    # Prepare the new random version number using random number generation
    new_version = f"{random.randint(1, 99999999)}"
    # Read the file
    lines = f.read()
    # Identify the tester for minimum traceability (username and hostname)
    tester = os.environ["GH_USERNAME"] + "@" + os.uname()[1]
    # Find and replace the line with 'version = "*"', a regex is enough
    new_lines = re.sub(rb'version = "[^"]*"',
                       f'version = "0.1.0-{new_version}+autotest_by_{tester}"'.encode(),
                       lines)
    # Write back the file
    with open("alire.toml", "wb") as f:
        f.write(new_lines)

# Commit changes
git_init_user()
subprocess.run(["git", "commit", "-a", "-m 'Unique version'"]).check_returncode()
subprocess.run(["git", "push"]).check_returncode()

# Publish; we need to force to skip the check that the source is remote
p = run_alr("publish", "--skip-build", quiet=False, force=True)

# Identify the PR number just created in a message like:
# Visit https://github.com/alire-project/test-index/pull/7 for details
# We use a regex to extract the number:
pr = re.search(r'/pull/(\d+) for details', p.out).group(1)

# Wait for the checks to complete. In the test index, there is only a mock test
# that always succeeds quickly. This allows us to check the status command too.
waited  = 0
timeout = 30
while True:
    p = run_alr("publish", "--status")
    line = [l for l in p.out.splitlines() if pr == l.split()[0]][0].lower()
    if "checks_passed" in line:
        break
    elif "checks_failed" in line:
        assert False, f"Checks failed unexpectedly for pr {pr}: {p.out}"
    elif waited > timeout:
        assert False, f"Checks not completed after {timeout} seconds for pr {pr}"
    else:
        # Wait a bit and retry, but fail after so many time
        time.sleep(1)
        waited += 1

# Request a review
run_alr("publish", f"--request-review={pr}")

# Cancel the PR to leave things as they were before the test
run_alr("publish", f"--cancel={pr}", "--reason='automated testing'")

print("SUCCESS")
