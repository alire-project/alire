"""
Check pinning to branches with "ssh://", "git+ssh://" and "git+https://" urls
"""

import os
import shutil
import subprocess

from drivers.alr import alr_pin, alr_unpin, init_local_crate
from drivers.helpers import init_git_repo, git_branch, MockGit
from drivers.asserts import assert_eq


# Create a crate with differing branches.
init_local_crate(name="remote", enter=False)
remote_path = os.path.join(os.getcwd(), "remote")
# On the default branch, test_file contains "This is the main branch.\n".
test_file_path = os.path.join(remote_path, "test_file")
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


# Perform the actual tests
urls = [
    "git+ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "git+ssh://ssh.gitlab.company-name.com/path/to/repo",
    "git+https://github.com/path/to/repo.git",
    # Should recognize URLs with ".git" suffix (without "git+" prefix)
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git/",
    "https://some.host/path/to/repo.git",
    # Should recognize github.com even without ".git" or "git+"
    "https://github.com/path/to/repo",
]
sanitised_urls = [
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo",
    "https://github.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git",
    "ssh://ssh.gitlab.company-name.com/path/to/repo.git/",
    "https://some.host/path/to/repo.git",
    "https://github.com/path/to/repo",
]
cache_test_file_path = "alire/cache/pins/remote/test_file"
mocked_git_dir = os.path.join(os.getcwd(), "mock_path")
for url, s_url in zip(urls, sanitised_urls):
    # Mock git with a wrapper that naively converts the url into the local path
    # to the "remote" crate.
    with MockGit({s_url: remote_path}, mocked_git_dir):
        # Create an empty crate, and pin the default branch of the test repo
        init_local_crate()
        alr_pin("remote", url=url, branch=default_branch)
        with open(cache_test_file_path) as f:
            assert_eq("This is the main branch.\n", f.read())

        # Edit pin to point to the other branch, and verify the cached copy changes
        # as it should
        alr_unpin("remote", update=False)
        alr_pin("remote", url=url, branch="other")
        with open(cache_test_file_path) as f:
            assert_eq("This is the other branch.\n", f.read())

    # Clean up for next test
    os.chdir("..")
    shutil.rmtree("xxx")


print("SUCCESS")
