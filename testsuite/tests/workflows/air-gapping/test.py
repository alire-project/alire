"""
Test fetching a crate online, and subsequently building offline.
"""


import contextlib
import os
import shutil
import subprocess

from drivers.alr import run_alr, alr_settings_set
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import MockCommand, on_windows


TOOLS = ("tar", "git", "hg", "svn", "curl", "gprbuild")
MOCK_DIR = os.path.join(os.getcwd(), "path-dir")
DUMMY_SCRIPT = """\
import sys
print('Mocked command called')
sys.exit(1)
"""


# Mock tar, git, curl, gprbuild etc. with a dummy script.
mock_commands = {}
for tool in TOOLS:
    mock_commands[tool] = MockCommand(tool, DUMMY_SCRIPT, MOCK_DIR)
    mock_commands[tool].enable()
# Changes to `PATH` are overridden in the case of MSYS2 tools, so we also
# redirect `msys2.install_dir` to an empty directory.
if on_windows():
    msys2_dir = run_alr("settings", "--global", "msys2.install_dir").out.strip()
    msys2_dir = msys2_dir.removeprefix("msys2.install_dir=")
    empty_dir = os.path.join(os.getcwd(), "empty-dir")
    alr_settings_set("msys2.install_dir", empty_dir)

# Run `alr get hello`. This will fail because tar is unavailable.
p = run_alr("get", "hello", quiet=False, complain_on_error=False)
assert_match(".*Mocked command called", p.out)
assert_match(".*Deployment of path .* to .* failed", p.out)

# Disable tar mocking. If tar is provided by msys2, we need to make it available
# explicitly because we have redirected `msys2.install_dir`.
mock_commands["tar"].disable()
msys2_tar = os.path.join(msys2_dir, "usr", "bin", "tar.exe") if on_windows() else ""
if on_windows() and os.path.isfile(msys2_tar):
    unmocked_tar = MockCommand(
        "tar",
        (
            "import subprocess, sys\n"
            f"sys.exit(subprocess.run([r'{msys2_tar}', *sys.argv[1:]]).returncode)"
        ),
        MOCK_DIR
    )
else:
    unmocked_tar = contextlib.nullcontext()
# With tar mocking disabled, run `alr get hello` to 'download' the crate and its
# dependencies.
with unmocked_tar:
    p = run_alr("get", "hello", quiet=False)
    assert_match(r".*hello=1\.0\.1 successfully retrieved", p.out)
    assert_match(r".*\+ libhello 1\.0\.0 \(new\)", p.out)

# Re-enable tar mocking and make the index unavailable to simulate disconnection
# from the network
mock_commands["tar"].enable()
shutil.move("my_index", "somewhere_else")

# Simulate transferring to a different system by clearing the alr-config
# directory (we keep settings.toml, since it just does various things to isolate
# the test environment) and changing the absolute path of the crate directory.
for f in os.listdir("alr-config"):
    if f != "settings.toml":
        shutil.rmtree(os.path.join("alr-config", f))
shutil.move(f"hello_1.0.1_filesystem", "hello")

# Run `alr build`. This will fail because gprbuild is unavailable.
os.chdir(f"hello")
p = run_alr("build", quiet=False, complain_on_error=False)
assert_match(".*Mocked command called", p.out)
assert_match(r'.*Command \["gprbuild", .*\] exited with code 1', p.out)

# Disable gprbuild mocking and run `alr build` to build the crate (with tar
# mocking still enabled to check it doesn't try to fetch anything else)
mock_commands["gprbuild"].disable()
p = run_alr("build", quiet=False)

# Check the built binary works as expected
assert_eq(
    ("Hello, world!" + ("\r\n" if on_windows() else "\n")).encode(),
    subprocess.run([os.path.join("obj", "hello")], capture_output=True).stdout
)

# Clear out the downloaded dependencies, and verify that `alr build` then
# attempts (and fails) to re-fetch them
shutil.rmtree(os.path.join("alire", "cache", "dependencies"))
p = run_alr("build", quiet=False, complain_on_error=False)
assert_match(
    ".*Filesystem crate is neither a folder nor a source archive: ",
    p.out
)
assert_match(".*Deployment of path .* to .* failed", p.out)


print("SUCCESS")
