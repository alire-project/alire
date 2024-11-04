"""
Check updating a branch pin when 'config/*' files are tracked.
"""


import os
import subprocess

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_match, assert_in_file
from drivers.helpers import git_branch, git_commit_file, init_git_repo


# Create crate yyy, with git tracking the 'config/*' files
init_local_crate("yyy")
yyy_path = os.getcwd()
with open(".gitignore") as f:
    gitignore_content = f.read()
gitignore_content = gitignore_content.replace("/config/\n", "")
with open(".gitignore", "w") as f:
    f.write(gitignore_content)
init_git_repo(".")
default_branch = git_branch()
os.chdir("..")

# Create and build another crate, with yyy's default branch added as a pin
init_local_crate()
xxx_path = os.getcwd()
alr_pin("yyy", url=f"git+file:{yyy_path}", branch=default_branch)
run_alr("build")

# Verify that the cached copy of yyy has a dirty repo (due to changes to the
# 'config/*' files)
cached_yyy_path = os.path.join(xxx_path, "alire", "cache", "pins", "yyy")
os.chdir(cached_yyy_path)
p = subprocess.run(["git", "status"], capture_output=True)
p.check_returncode()
assert_match(r".*modified:\s*config/yyy_config\.gpr", p.stdout.decode())

# Add commits to yyy's default branch, including a change to a file in 'config/'
os.chdir(yyy_path)
git_commit_file(
    "Change_config", "config/yyy_config.gpr", "This is a new addition\n", "a"
)
git_commit_file("Add_test_file", "test_file", "This is a new file\n")

# Check that the dirty repo doesn't prevent updating the pin
os.chdir(xxx_path)
run_alr("update")

# Check that the update was successful
assert_in_file(os.path.join(cached_yyy_path, "test_file"), "This is a new file")


print('SUCCESS')
