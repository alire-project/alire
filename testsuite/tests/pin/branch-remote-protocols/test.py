"""
Check pinning to branches with "ssh://", "git+ssh://" and "git+https://" urls
"""

import os
import subprocess

from drivers.alr import alr_pin, alr_unpin, init_local_crate
from drivers.helpers import init_git_repo, git_branch
from drivers.asserts import assert_eq


# Create a crate with differing branches.
init_local_crate(name="remote", enter=False)
LOCAL_REPO_PATH = os.path.join(os.getcwd(), "remote")
# On the default branch, test_file contains "This is the main branch.\n".
test_file_path = os.path.join(LOCAL_REPO_PATH, "test_file")
with open(test_file_path, "w") as f:
    f.write("This is the main branch.\n")
init_git_repo("remote")
os.chdir("remote")
default_branch = git_branch()
# On the "other" branch, test_file contains "This is the other branch.\n".
subprocess.run(["git", "checkout", "-b", "other"]).check_returncode()
with open(test_file_path, "w") as f:
    f.write("This is the other branch.\n")
subprocess.run(["git", "add", "test_file"]).check_returncode()
subprocess.run(["git", "commit", "-m", "Change test_file"]).check_returncode()
# Return to the default branch
subprocess.run(["git", "checkout", default_branch]).check_returncode()
os.chdir("..")


# Prepare a directory on PATH at which to mock git.
ACTUAL_GIT_PATH = (
    subprocess.run(["bash", "-c", "type -p git"], capture_output=True)
    .stdout.decode()
    .strip()
)
MOCK_PATH = os.path.join(os.getcwd(), "mock_path")
os.mkdir(MOCK_PATH)
os.environ["PATH"] = f'{MOCK_PATH}:{os.environ["PATH"]}'


# Perform the actual tests
URLs = [
    "git+ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "git+ssh://ssh.gitlab.company-name.com/path/to/repo",
    "git+https://github.com/path/to/repo.git",
    # Should recognise URLs with ".git" suffix (without "git+" prefix)
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git/",
    "https://some.host/path/to/repo.git",
    # Should recognise github.com even without ".git" or "git+"
    "https://github.com/path/to/repo",
]
SANITISED_URLS = [
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo",
    "https://github.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git/",
    "https://some.host/path/to/repo.git",
    "https://github.com/path/to/repo",
]
CACHE_TEST_FILE_PATH = "alire/cache/pins/remote/test_file"
for URL, S_URL in zip(URLs, SANITISED_URLS):
    # Mock git with a wrapper that naively converts the url into the local path
    # to the "remote" crate.
    wrapper_script = "\n".join(
        [
            "#! /usr/bin/env python",
            "import subprocess, sys",
            'if sys.argv[1:] == ["config", "--list"]:',
            f'    print("remote.origin.url={S_URL}\\n")',
            "else:",
            "    args = [",
            f'        ("{LOCAL_REPO_PATH}" if a == "{S_URL}" else a)',
            "        for a in sys.argv[1:]",
            "    ]",
            f'    subprocess.run(["{ACTUAL_GIT_PATH}"] + args).check_returncode()',
        ]
    )
    wrapper_descriptor = os.open(
        os.path.join(MOCK_PATH, "git"),
        flags=(os.O_WRONLY | os.O_CREAT | os.O_TRUNC),
        mode=0o764,
    )
    with open(wrapper_descriptor, "w") as f:
        f.write(wrapper_script)

    # Create an empty crate, and pin the default branch of the test repo
    init_local_crate()
    alr_pin("remote", url=URL, branch=default_branch)
    with open(CACHE_TEST_FILE_PATH) as f:
        assert_eq("This is the main branch.\n", f.read())

    # Edit pin to point to the other branch, and verify the cached copy changes
    # as it should
    alr_unpin("remote", update=False)
    alr_pin("remote", url=URL, branch="other")
    with open(CACHE_TEST_FILE_PATH) as f:
        assert_eq("This is the other branch.\n", f.read())


# Restore PATH
os.environ["PATH"] = os.environ["PATH"][len(MOCK_PATH) + 1 :]


print("SUCCESS")
