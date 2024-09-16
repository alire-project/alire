"""
Test fetching a crate online, and subsequently building offline.
"""


import os
import shutil
import subprocess
import sys

from drivers.alr import run_alr
from drivers.helpers import init_git_repo, on_windows
from drivers.asserts import assert_eq, assert_match


# Mock git, curl, gprbuild etc. with dummy scripts
if on_windows():
    print('SKIP: command mocking unavailable on Windows')
    sys.exit(0)
os.mkdir("path-dir")
os.chdir("path-dir")
for executable in ("git", "hg", "svn", "curl", "gprbuild"):
    with open(executable, "w") as f:
        f.write("\n".join([
            "#!/usr/bin/env python",
            "import sys",
            "print('Mocked command called')",
            "sys.exit(1)"
        ]))
    os.chmod(executable, 0o764)
os.environ["PATH"] = f'{os.getcwd()}{os.pathsep}{os.environ["PATH"]}'
os.chdir("..")


# 'dependencies.shared=false' is required to ensure dependencies are packaged
# inside the workspace (at ./alire/cache/dependencies)
run_alr("settings", "--global", "--set", "dependencies.shared", "false")

# Run `alr get hello`. This will fail because git is unavailable.
p = run_alr("get", "hello", quiet=False, complain_on_error=False)
assert_match(".*Mocked command called", p.out)
assert_match(".*Deployment of commit .* from .* to .* failed", p.out)

# Disable git mocking and run `alr get hello` to 'download' the crate and its
# dependencies.
os.remove(os.path.join("path-dir", "git"))
p = run_alr("get", "hello", quiet=False)
assert_match(r".*hello=1\.0\.1 successfully retrieved", p.out)
assert_match(r".*\+ libhello 1\.0\.0 \(new\)", p.out)

# Re-enable git mocking and make the index unavailable to simulate disconnection
# from the network
shutil.copy(os.path.join("path-dir", "curl"), os.path.join("path-dir", "git"))
shutil.move("my_index", "somewhere_else")

# Simulate transferring to a different system by clearing the alr-config
# directory (we keep settings.toml, since it just does various things to isolate
# the test environment) and changing the absolute path of the crate directory.
for f in os.listdir("alr-config"):
    if f != "settings.toml":
        shutil.rmtree(os.path.join("alr-config", f))
shutil.move(f"hello_1.0.1_cd89b04b", "hello")

# Run `alr build`. This will fail because gprbuild is unavailable.
os.chdir(f"hello")
p = run_alr("build", quiet=False, complain_on_error=False)
assert_match(".*Mocked command called", p.out)
assert_match(r'.*Command \["gprbuild", .*\] exited with code 1', p.out)

# Disable gprbuild mocking and run `alr build` to build the crate (with git
# mocking still enabled to check it doesn't try to fetch anything else)
os.remove(os.path.join("..", "path-dir", "gprbuild"))
p = run_alr("build", quiet=False)
assert_match(".*Build finished successfully in .* seconds", p.out)

# Check the built binary works as expected
assert_eq(
    b"Hello, world!\n",
    subprocess.run([os.path.join("obj", "hello")], capture_output=True).stdout
)

# Clear out the downloaded dependencies, and verify that `alr build` then
# attempts (and fails) to re-fetch them
shutil.rmtree(os.path.join("alire", "cache", "dependencies"))
p = run_alr("build", quiet=False, complain_on_error=False)
assert_match(".*Mocked command called", p.out)
assert_match(".*Deployment of commit .* from .* to .* failed", p.out)


print("SUCCESS")
