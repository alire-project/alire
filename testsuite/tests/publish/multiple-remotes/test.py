"""
Check publishing from a local repo with multiple remotes configured.
"""


import os
import re
import subprocess

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from drivers.helpers import git_branch, git_head, init_git_repo


TEST_ROOT_DIR = os.getcwd()

COMMIT_CONFIRM_PROMPT = (
    r".*The specified branch is not configured to track a remote\.(\r\n|\r|\n)"
    r"Do you want to attempt to publish its head commit anyway\?"
)
COMMIT_NOT_PRESENT_MESSAGE = (
    r".*The specified commit is not present on any configured remote\."
)
URL_CONFIRM_PROMPT_TEMPLATE = (
    r".*The repository has multiple remotes configured, but the specified "
    r"commit is only present on the remote 'remote{n}'\.(\r\n|\r|\n)"
    r"The published manifest will therefore use the origin "
    rf"'{re.escape(TEST_ROOT_DIR + os.path.sep)}remote{{n}}'\.(\r\n|\r|\n)"
    r"Is this correct\?"
)
AMBIGUOUS_REMOTE_MESSAGE = (
    r".*The specified commit is present on multiple remotes.*Please use "
    r"'alr publish <remote-URL> <commit>' to resolve this ambiguity\."
)
UNSTAGED_CHANGES_MESSAGE = (
    r".*You have unstaged changes\. Please commit or stash them\."
)
AHEAD_OF_REMOTE_MESSAGE = (
    r".*Your branch is ahead of remote.*Please push local commits to the "
    r"remote branch\."
)
BEHIND_REMOTE_CONFIRM_PROMPT = (
    r".*There are commits on the remote branch which could be retrieved with "
    r"'git pull'\.(\r\n|\r|\n)Are you sure you want to publish without them\?"
)


def run(*args, **kwargs):
    subprocess.run(*args, **kwargs).check_returncode()

def commit_file(commit_name: str, path: str, content: str):
    """
    Create a new file with the specified content and `git commit` it.

    Also returns the commit's hash and attaches the tag `f"tag_{commit_name}"`
    thereto.
    """
    with open(path, "x") as f:
        f.write(content)
    run(["git", "add", path])
    run(["git", "commit", "-m", f"Commit {commit_name}"])
    run(["git", "tag", f"tag_{commit_name}"])
    return git_head()

def test_publishing(
    extra_args: list[str],
    remote: int,
    commit_id: str,
    output_pattern: str | None = None,
):
    """
    Run `alr --force publish --skip-submit` with the specified additional args,
    and assert that it succeeds (optionally asserting the output matches a regex
    pattern) and that the resulting manifest points to the specified remote
    and commit.
    """
    p = run_alr("--force", "publish", "--skip-submit", *extra_args, quiet=False)
    manifest_path = os.path.join(
        TEST_ROOT_DIR, "xxx", "alire", "releases", "xxx-0.1.0-dev.toml"
    )
    assert_match(
        (
            ".*Your index manifest file has been generated at "
            + re.escape(manifest_path)
        ),
        p.out
    )
    if output_pattern is not None:
        assert_match(output_pattern, p.out)
    with open(manifest_path) as f:
        manifest = f.read()
    assert_match(rf'.*url = "git\+file:.*remote{remote}"', manifest)
    assert_match(rf'.*commit = "{commit_id}"', manifest)

def check_publishing_fails(extra_args: list[str], error_pattern: str | None):
    """
    Run `alr --force publish --skip-submit` with the specified additional args,
    and assert that it fails (optionally asserting the output matches a regex
    pattern).
    """
    p = run_alr(
        "--force", "publish", "--skip-submit", *extra_args,
        quiet=False, complain_on_error=False
    )
    if error_pattern is not None:
        assert_match(error_pattern, p.out)


# Create two empty 'remote' repositories.
os.mkdir("remote1")
run(["git", "init", "--bare", "remote1"])
remote1_path = os.path.join(TEST_ROOT_DIR, "remote1")
os.mkdir("remote2")
run(["git", "init", "--bare", "remote2"])
remote2_path = os.path.join(TEST_ROOT_DIR, "remote2")
# Create a 'local' repo containing a crate.
init_local_crate(with_maintainer_login=True)
initial_commit = init_git_repo(".")
default_branch = git_branch()
# Create a new branch.
run(["git", "checkout", "-b", "other_branch"])
# Add various commits (and tags) to both branches, including a merge commit.
#
# The repo history will be:
#     A - B - C - D  <--- default branch
#       \   /
#         E - F  <--- other_branch
commit_A = initial_commit
run(["git", "tag", "tag_A"])
commit_E = commit_file(
    commit_name="E",
    path="test_file_0",
    content="This file will be merged so that it is on both branches.\n"
)
run(["git", "checkout", default_branch])
commit_B = commit_file(
    commit_name="B",
    path="test_file_1",
    content="This file is only on the default branch.\n"
)
run(["git", "merge", "other_branch"])
commit_C = git_head()
run(["git", "tag", "tag_C"])
commit_D = commit_file(
    commit_name="D",
    path="test_file_2",
    content="This file is also only on the default branch.\n"
)
run(["git", "checkout", "other_branch"])
commit_F = commit_file(
    commit_name="F",
    path="test_file_3",
    content="This file is only on other_branch.\n"
)
# Add branches at the same locations but which are not configured to track
# either remote.
run(["git", "branch", "other_branch_local"])
run(["git", "checkout", default_branch])
run(["git", "branch", "default_branch_local"])


# Check `alr publish` gives an appropriate error with no remotes configured.
check_publishing_fails(extra_args=[], error_pattern=".*No remote configured")


# Set the default branch to track remote1.
run(["git", "remote", "add", "remote1", remote1_path])
run(["git", "push", "--set-upstream", "remote1", default_branch])

# Verify `alr publish` still fails with other_branch checked out but succeeds
# with the default branch.
run(["git", "checkout", "other_branch"])
check_publishing_fails(
    extra_args=[],
    error_pattern=COMMIT_CONFIRM_PROMPT + COMMIT_NOT_PRESENT_MESSAGE
)
run(["git", "checkout", default_branch])
test_publishing(extra_args=[], remote=1, commit_id=commit_D)
# Verify the same results with the form `alr publish . <branch>`.
check_publishing_fails(
    extra_args=[".", "other_branch"],
    error_pattern=COMMIT_CONFIRM_PROMPT + COMMIT_NOT_PRESENT_MESSAGE
)
test_publishing(extra_args=[".", default_branch], remote=1, commit_id=commit_D)
# Verify publishing default_branch_local also succeeds, subject to user
# confirmation.
test_publishing(
    extra_args=[".", "default_branch_local"],
    remote=1,
    commit_id=commit_D,
    output_pattern=COMMIT_CONFIRM_PROMPT
)
run(["git", "checkout", "default_branch_local"])
test_publishing(
    extra_args=[],
    remote=1,
    commit_id=commit_D,
    output_pattern=COMMIT_CONFIRM_PROMPT
)

# Verify `alr publish . <commit>` succeeds for all commits present on the remote
# and fails for commit_F (which has not been pushed).
commits = [commit_A, commit_B, commit_C, commit_D, commit_E]
tags =    ["tag_A",  "tag_B",  "tag_C",  "tag_D",  "tag_E" ]
for commit in commits:
    test_publishing(extra_args=[".", commit], remote=1, commit_id=commit)
check_publishing_fails(
    extra_args=[".", commit_F], error_pattern=COMMIT_NOT_PRESENT_MESSAGE
)
# Verify the same results for tags.
for commit, tag in zip(commits, tags):
    test_publishing(extra_args=[".", tag], remote=1, commit_id=commit)
check_publishing_fails(
    extra_args=[".", "tag_F"], error_pattern=COMMIT_NOT_PRESENT_MESSAGE
)


# Set other_branch to track remote2, so the repo now has two remotes
run(["git", "remote", "add", "remote2", remote2_path])
run(["git", "push", "--set-upstream", "remote2", "other_branch"])

# Verify publishing with other_branch checked out now yields a manifest pointing
# to remote2_path.
run(["git", "checkout", "other_branch"])
test_publishing(extra_args=[], remote=2, commit_id=commit_F)
# Check out the default branch, and verify publishing still yields a manifest
# pointing to remote1_path.
run(["git", "checkout", default_branch])
test_publishing(extra_args=[], remote=1, commit_id=commit_D)
# Verify the same results with the form `alr publish . <branch>`
test_publishing(extra_args=[".", "other_branch"], remote=2, commit_id=commit_F)
test_publishing(extra_args=[".", default_branch], remote=1, commit_id=commit_D)

# Verify publishing a commit (or tag) which is on only one remote succeeds,
# subject to user confirmation.
commits = [commit_B, commit_C, commit_D, commit_F]
tags =    ["tag_B",  "tag_C",  "tag_D",  "tag_F" ]
remotes = [1,        1,        1,        2       ]
for commit, tag, remote in zip(commits, tags, remotes):
    for ref in [commit, tag]:
        test_publishing(
            extra_args=[".", ref],
            remote=remote,
            commit_id=commit,
            output_pattern=URL_CONFIRM_PROMPT_TEMPLATE.format(n=remote)
        )
# Verify publishing a commit (or tag) which is on multiple remotes fails, with
# instructions on resolving the ambiguity. Also verify that those instructions
# are correct (i.e. that `alr publish <remote-URL> <commit>` succeeds).
for ref in [commit_E, "tag_E"]:
    check_publishing_fails(
        extra_args=[".", ref], error_pattern=AMBIGUOUS_REMOTE_MESSAGE,
    )
    test_publishing([f"git+file:{remote1_path}", commit_E], 1, commit_E)
    test_publishing([f"git+file:{remote2_path}", commit_E], 2, commit_E)

# Verify publishing the branches which are not pushed to the remote succeeds,
# subject to user confirmation.
cases = [
    ("default_branch_local", commit_D, 1), ("other_branch_local", commit_F, 2)
]
for branch, commit, remote in cases:
    test_publishing(
        extra_args=[".", branch],
        remote=remote,
        commit_id=commit,
        output_pattern=(
            COMMIT_CONFIRM_PROMPT + URL_CONFIRM_PROMPT_TEMPLATE.format(n=remote)
        )
    )
# Verify `alr publish` with the branch checked out yields the same result.
for branch, commit, remote in cases:
    run(["git", "checkout", branch])
    test_publishing(
        extra_args=[],
        remote=remote,
        commit_id=commit,
        output_pattern=(
            COMMIT_CONFIRM_PROMPT + URL_CONFIRM_PROMPT_TEMPLATE.format(n=remote)
        )
    )

# Add a new branch which points to commit E (present on both remotes), and check
# publishing fails with both a prompt that the branch isn't tracking a remote
# and instructions on resolving the ambiguity.
run(["git", "branch", "ambiguous_branch", commit_E])
check_publishing_fails(
    extra_args=[".", "ambiguous_branch"],
    error_pattern=COMMIT_CONFIRM_PROMPT + AMBIGUOUS_REMOTE_MESSAGE,
)
run(["git", "checkout", "ambiguous_branch"])
check_publishing_fails(
    extra_args=[],
    error_pattern=COMMIT_CONFIRM_PROMPT + AMBIGUOUS_REMOTE_MESSAGE,
)

# Configure ambiguous_branch to track one of the remotes, and verify publishing
# now succeeds and uses this remote as the origin.
run(["git", "push", "--set-upstream", "remote2", "ambiguous_branch"])
test_publishing(extra_args=[], remote=2, commit_id=commit_E)
run(["git", "checkout", default_branch])
test_publishing(
    extra_args=[".", "ambiguous_branch"], remote=2, commit_id=commit_E
)

# Verify that user confirmation is required to publish a local branch which is
# behind the remote branch (the remote branch points to commit_D, but the local
# branch points to commit_C).
run(["git", "checkout", "-b", "behind_branch", commit_D])
run(["git", "push", "--set-upstream", "remote1", "behind_branch"])
run(["git", "reset", "--hard", commit_C])
test_publishing(
    extra_args=[],
    remote=1,
    commit_id=commit_C,
    output_pattern=BEHIND_REMOTE_CONFIRM_PROMPT
)
# Verify the same result specifying the branch explicitly
run(["git", "checkout", "other_branch"])
test_publishing(
    extra_args=[".", "behind_branch"],
    remote=1,
    commit_id=commit_C,
    output_pattern=BEHIND_REMOTE_CONFIRM_PROMPT
)

# Edit one of the files and check that `alr publish` fails due to the unclean
# repo.
run(["git", "checkout", default_branch])
with open("test_file_1", "w") as f:
    f.write("This file is only on the default branch, and has been edited.\n")
check_publishing_fails(extra_args=[], error_pattern=UNSTAGED_CHANGES_MESSAGE)
# Verify the same failure for explicitly specifying any branch, commit or tag
refs = [
    default_branch, "other_branch", "default_branch_local",
    commit_D, commit_E, commit_F, "tag_D", "tag_E", "tag_F",
]
for ref in refs:
    check_publishing_fails(
        extra_args=[".", ref], error_pattern=UNSTAGED_CHANGES_MESSAGE
    )

# Commit the edited file and check that `alr publish` still fails because the
# remote is not synchronised.
run(["git", "add", "test_file_1"])
run(["git", "commit", "-m", "This commit is on default branch but not remote1"])
commit_G = git_head()
check_publishing_fails(extra_args=[], error_pattern=AHEAD_OF_REMOTE_MESSAGE)
# Explicitly publishing the unsynchronised branch or commit should also fail
# (regardless of which branch is currently checked out).
check_publishing_fails(
    extra_args=[".", default_branch], error_pattern=AHEAD_OF_REMOTE_MESSAGE
)
check_publishing_fails(
    extra_args=[".", commit_G], error_pattern=COMMIT_NOT_PRESENT_MESSAGE
)
run(["git", "checkout", "other_branch"])
check_publishing_fails(
    extra_args=[".", default_branch], error_pattern=AHEAD_OF_REMOTE_MESSAGE
)
check_publishing_fails(
    extra_args=[".", commit_G], error_pattern=COMMIT_NOT_PRESENT_MESSAGE
)
# However, publishing other commits/tags/branches should now work again, since
# they are present on the relevant remote.
test_publishing(extra_args=[], remote=2, commit_id=commit_F)
cases = [
    (commit_B, commit_B, 1), (commit_D, commit_D, 1), (commit_F, commit_F, 2),
    ("tag_B", commit_B, 1), ("tag_D", commit_D, 1), ("tag_F", commit_F, 2),
    ("other_branch", commit_F, 2), ("ambiguous_branch", commit_E, 2)
]
for ref, commit, remote in cases:
    test_publishing(extra_args=[".", ref], remote=remote, commit_id=commit)

# Disable the default branch's tracking of remote1, and verify that publishing
# fails with appropriate messages.
run(["git", "checkout", default_branch])
run(["git", "branch", "--unset-upstream"])
check_publishing_fails(
    extra_args=[],
    error_pattern=COMMIT_CONFIRM_PROMPT + COMMIT_NOT_PRESENT_MESSAGE
)
check_publishing_fails(
    extra_args=[".", default_branch],
    error_pattern=COMMIT_CONFIRM_PROMPT + COMMIT_NOT_PRESENT_MESSAGE
)

# Verify that a detached head works, and behaves the same as specifying the
# commit explicitly
run(["git", "checkout", commit_B])
test_publishing(
    extra_args=[],
    remote=1,
    commit_id=commit_B,
    output_pattern=URL_CONFIRM_PROMPT_TEMPLATE.format(n=1)
)
run(["git", "checkout", commit_E])
check_publishing_fails(extra_args=[], error_pattern=AMBIGUOUS_REMOTE_MESSAGE)
run(["git", "checkout", commit_G])
check_publishing_fails(extra_args=[], error_pattern=COMMIT_NOT_PRESENT_MESSAGE)


print('SUCCESS')
