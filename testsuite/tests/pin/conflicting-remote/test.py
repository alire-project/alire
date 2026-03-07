"""
Check conflict detection for remote pins for the same crate
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import git_blast, git_commit_file, git_head, init_git_repo
from re import escape

import re
import os
import shutil
import subprocess

#  "remote" is going to be the remote crate. Two other crates will depend on it
#  with different branches/commits etc

init_local_crate(name="zzz", enter=False)
url = os.path.join(os.getcwd(), "zzz")
head1 = init_git_repo("zzz")
os.chdir("zzz")

# Create a second branch and commit for testing
subprocess.run(["git", "checkout", "-b", "devel"]).check_returncode()
head2 = git_commit_file(commit_name="branching", path="x", content="")
os.chdir("..")


#  In this function we will test that two crates with the same remote works,
#  for several combinations
def should_work(commit="", branch=""):
    os.mkdir("nest")
    os.chdir("nest")

    for crate in ["xxx", "yyy"]:
        init_local_crate(crate)
        alr_pin("zzz", url=url, commit=commit, branch=branch)
        os.chdir("..")

    os.chdir("xxx")
    alr_pin("yyy", path="../yyy")

    p = run_alr("pin")
    assert_match(escape("yyy file:../yyy") + ".*\n" +
                 escape("zzz file:alire/cache/pins/zzz") + ".*" +
                 escape(url),
                 p.out)

    # Clean up for next trial
    os.chdir("..")
    os.chdir("..")
    git_blast("nest")


#  In this function we will test conflicts that should be detected
def should_not_work(commits=["", ""], branches=["", ""], match_error="FAIL"):
    #  Commits and branches must contain two values that go into each crate pin

    os.mkdir("nest")
    os.chdir("nest")
    crates = ["xxx", "yyy"]

    for i in [0, 1]:
        init_local_crate(crates[i])
        alr_pin("zzz", url=url, commit=commits[i], branch=branches[i])
        os.chdir("..")

    os.chdir("xxx")
    alr_pin("yyy", path="../yyy", update=False)

    p = run_alr("pin", complain_on_error=False)
    assert_match(match_error,
                 p.out)

    # Clean up for next trial
    os.chdir("..")
    os.chdir("..")
    shutil.rmtree("nest")


should_work()                # Remote at default branch
should_work(commit=head1)    # Remote at given commit
should_work(branch="devel")  # Remote at given branch

should_not_work(commits=[head1, head2],
                match_error=".*" +
                re.escape("Conflicting pin links for crate zzz: "
                          "Crate xxx wants to link ") +
                ".*" + re.escape(", but a previous link exists to ") +
                ".*zzz#[0-9a-f]+.*")  # with commit
should_not_work(branches=["", "devel"],
                match_error=".*" +
                re.escape("Conflicting pin links for crate zzz: "
                          "Crate xxx wants to link ") +
                ".*" + re.escape(", but a previous link exists to ") +
                ".*zzz#devel\n")  # with branch

print('SUCCESS')
