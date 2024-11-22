"""
Check "alr publish --for-private-index" supports private indexes
"""


import os
import re
import shutil
import subprocess

from drivers.alr import run_alr, run_alr_interactive
from drivers.helpers import init_git_repo, MockGit
from drivers.asserts import assert_match, assert_file_exists


INDEX_PATH = os.path.join(os.getcwd(), "my_index", "index")


def run(*args):
    subprocess.run(*args).check_returncode()

def test(
    args,
    url,
    num_confirms,
    output,
    gen_manifest=None,
    maint_logins=None,
    github_user=None,
    expect_success=True
):
    """
    Perform the general test procedure.

    - Create a mock remote repo which appears to have a remote URL, and a local
      clone thereof.
    - `alr init` a crate in this repo
    - Run `alr` with the specified arguments, responding `y` to the prompt
      `Do you want to proceed with this information?` a specified number of
      times
    - Assert that `alr`'s final output matches zero or more regex patterns
    - Optionally, assert that an index manifest was generated and matches zero
      or more regex patterns

    :param list(str) args: The arguments to pass to `alr` (`--no-color` will be
        added)
    :param str url: The URL at which (as far as Alire is concerned) the remote
        repository is located
    :param int num_confirms: The number of times to respond `y` to the prompt
        `Do you want to proceed with this information?`
    :param list(str) output: Zero or more regex patterns which must match the
        final output (i.e. that which follows the last confirmation prompt) of
        `alr`
    :param list(str) gen_manifest: Zero or more regex patterns which must match
        the content of the generated manifest. If None, expects no manifest to
        be generated.
    :param str maint_logins: If not `None`, the value to set for the
        `maintainers-logins` field in the crate's manifest before calling `alr`
    :param str github_user: If not `None`, the value to set as
        `user.github_login` before calling `alr`
    :param bool expect_success: If True, the test will fail if `alr` returns a
        non-zero exit code. If False, fail on a zero exit code.
    """
    # Create an alire workspace to act as a "remote"
    os.makedirs("remote")
    os.chdir("remote")
    run_alr("init", "--bin", "xxx")
    os.chdir("xxx")
    # Adjust the value of maintainers-logins if required
    if maint_logins is not None:
        with open("alire.toml", "a") as f:
            f.write(f"maintainers-logins = {maint_logins}\n")
    # Initialise as a git repo
    init_git_repo(".")
    remote_path = os.getcwd()

    # Mock git with a wrapper that naively converts the url into the local path
    # to the "remote" crate.
    mocked_git_dir = os.path.abspath(os.path.join("..", "..", "mocked_git"))
    with MockGit({url: remote_path}, mocked_git_dir):
        # Create a "local" clone of the "remote"
        local_path = os.path.abspath(os.path.join("..", "..", "local", "xxx"))
        os.makedirs(local_path)
        os.chdir(local_path)
        run(["git", "clone", url, local_path])
        # Adjust the value of user.github_login if required
        if github_user is not None:
            run_alr("settings", "--set", "user.github_login", github_user)

        # Run alr
        p = run_alr_interactive(
            args,
            output=num_confirms * [
                "Do you want to proceed with this information?"
            ],
            input=num_confirms * ["y"],
            complain_on_error=expect_success,
            timeout=60,
        )

        # Check output matches
        for pattern in output:
            assert_match(pattern, p)

        # Check the generated manifest file
        gen_manifest_path = os.path.join(
            os.getcwd(), "alire", "releases", "xxx-0.1.0-dev.toml"
        )
        idx_manifest_dir = os.path.join(INDEX_PATH, "xx", "xxx")
        os.chdir(os.path.join("..", ".."))
        if gen_manifest is None:
            assert_file_exists(gen_manifest_path, wanted=False)
        else:
            # Check existence
            assert_file_exists(gen_manifest_path, wanted=True)

            # Check regex matches
            with open(gen_manifest_path) as f:
                manifest = f.read()
            for pattern in gen_manifest:
                assert_match(pattern, manifest)

            # Add this manifest to our local index
            os.makedirs(idx_manifest_dir)
            shutil.copyfile(
                gen_manifest_path,
                os.path.join(idx_manifest_dir, "xxx-0.1.0-dev.toml")
            )

            # Check that the crate can be retrieved and built without error
            p = run_alr("get", "--build", "xxx", quiet=False)
            assert_match(
                r".*xxx=0\.1\.0-dev successfully retrieved and built.*",
                p.out
            )

    # Clean up for next test
    shutil.rmtree("local")
    shutil.rmtree("remote")
    shutil.rmtree(idx_manifest_dir, ignore_errors=True) # may not exist


# All tests should behave the same with and without "--force"
for force_arg in ([], ["--force"]):
    # A crate suitable for the community index:
    #
    # Publication should succeed, with either "--for-private-index" or
    # "--skip-submit" circumventing the requirement for the user to provide a
    # GitHub account with a fork of the community index.
    test(
        args=force_arg + ["publish", "--skip-submit"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["github-username"]',
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            # Even though the automatic pull request has been skipped, alr
            # should provide instructions for submission to the community index.
            (
                r".*Please create a pull request against the community index "
                r"at https://github.com/alire-project/alire-index including "
                r"this file at index/xx/xxx/.*"
            ),
        ],
        gen_manifest=[
            # "git+" should be prepended to avoid ambiguity
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )
    test(
        args=force_arg + ["publish", "--for-private-index"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["github-username"]',
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            # alr should provide instructions again, but they should be more
            # generic, since we don't know where the private index is located.
            r".*Please upload this file to the index in the xx/xxx/ subdirectory",
        ],
        gen_manifest=[
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )

    # A crate suitable for the community index, with a GitHub user configured:
    test(
        args=force_arg + ["publish", "--skip-submit"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["github-username"]',
        github_user="github-username",
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            # The user has configured a GitHub username, so a specific upload
            # URL should be provided
            (
                r".*If you haven't already, please fork "
                r"https://github.com/alire-project/alire-index to your GitHub.*"
            ),
            (
                r".*This file can then be uploaded to "
                r"https://github\.com/github-username/alire-index/upload/"
                r"stable-1\.3\.0/index/xx/xxx to create a pull request against"
                r" the community index.*"
            ),
        ],
        gen_manifest=[
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )

    # A crate unsuitable for the community index because its origin is private:
    private_urls = [
        "ssh://github.com/some_user/repo-name.git",
        "git@bitbucket.org:/some_user/repo-name.git",
        "https://user@github.com/some_user/repo-name.git",
        "https://user:pass@github.com/some_user/repo-name.git",
    ]
    for url in private_urls:
        # "alr publish" should fail, because the origin URL looks private (it
        # will also fail if the user does not provide a GitHub account with a
        # fork of the community index, but that check comes later).
        test(
            args=force_arg + ["publish"],
            url=url,
            maint_logins='["github-username"]',
            num_confirms=1, # (fails before second confirmation)
            output=[
                r".*The origin cannot use a private remote:.*",
            ],
            gen_manifest=None,
            expect_success=False
        )
        # "alr publish --skip-submit" will fail for the same reason.
        test(
            args=force_arg + ["publish", "--skip-submit"],
            url=url,
            maint_logins='["github-username"]',
            num_confirms=1,
            output=[
                r".*The origin cannot use a private remote:.*",
            ],
            gen_manifest=None,
            expect_success=False
        )
        # "alr publish --for-private-index" will succeed.
        test(
            args=force_arg + ["publish", "--for-private-index"],
            url=url,
            maint_logins='["github-username"]',
            num_confirms=2,
            output=[
                r".*Success: Your index manifest file has been generated.*",
                r".*Please upload this file to the index in the xx/xxx/ subdirectory",
            ],
            gen_manifest=[
                f'.*url = "{re.escape(url)}".*',
            ],
            expect_success=True
        )

    # A crate unsuitable for the community index because it has a
    # "maintainers-logins" value which is invalid for GitHub:
    #
    # "alr publish" and "alr publish --skip-submit" should fail.
    test(
        args=force_arg + ["publish"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["valid-for-GitHub", "invalid_for_GitHub"]',
        num_confirms=0, # (fails before first confirmation)
        output=[
            (
                r".*The maintainer login 'invalid_for_GitHub' "
                r"is not a valid GitHub username.*"
            ),
        ],
        gen_manifest=None,
        expect_success=False
    )
    test(
        args=force_arg + ["publish", "--skip-submit"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["valid-for-GitHub", "invalid_for_GitHub"]',
        num_confirms=0,
        output=[
            (
                r".*The maintainer login 'invalid_for_GitHub' "
                r"is not a valid GitHub username.*"
            ),
        ],
        gen_manifest=None,
        expect_success=False
    )
    # "alr publish --for-private-index" will succeed.
    test(
        args=force_arg + ["publish", "--for-private-index"],
        url="https://github.com/some_user/repo-name",
        maint_logins='["valid-for-GitHub", "invalid_for_GitHub"]',
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            r".*Please upload this file to the index in the xx/xxx/ subdirectory",
        ],
        gen_manifest=[
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )

    # A crate unsuitable for the community index because it has no
    # "maintainers-logins" value:
    #
    # "alr publish" and "alr publish --skip-submit" should fail.
    test(
        args=force_arg + ["publish"],
        url="https://github.com/some_user/repo-name",
        maint_logins=None,
        num_confirms=0, # (fails before first confirmation)
        output=[
            r".*Missing required properties: maintainers-logins.*",
        ],
        gen_manifest=None,
        expect_success=False
    )
    test(
        args=force_arg + ["publish", "--skip-submit"],
        url="https://github.com/some_user/repo-name",
        maint_logins=None,
        num_confirms=0,
        output=[
            r".*Missing required properties: maintainers-logins.*",
        ],
        gen_manifest=None,
        expect_success=False
    )
    # "alr publish --for-private-index" will succeed.
    test(
        args=force_arg + ["publish", "--for-private-index"],
        url="https://github.com/some_user/repo-name",
        maint_logins=None,
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            r".*Please upload this file to the index in the xx/xxx/ subdirectory",
        ],
        gen_manifest=[
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )

    # A crate unsuitable for the community index because "maintainers-logins"
    # is an empty list:
    #
    # This should be identical to there being no "maintainers-logins" field at
    # all.
    test(
        args=force_arg + ["publish"],
        url="https://github.com/some_user/repo-name",
        maint_logins="[]",
        num_confirms=0,
        output=[
            r".*Missing required properties: maintainers-logins.*",
        ],
        gen_manifest=None,
        expect_success=False
    )
    test(
        args=force_arg + ["publish", "--skip-submit"],
        url="https://github.com/some_user/repo-name",
        maint_logins="[]",
        num_confirms=0,
        output=[
            r".*Missing required properties: maintainers-logins.*",
        ],
        gen_manifest=None,
        expect_success=False
    )
    test(
        args=force_arg + ["publish", "--for-private-index"],
        url="https://github.com/some_user/repo-name",
        maint_logins="[]",
        num_confirms=2,
        output=[
            r".*Success: Your index manifest file has been generated.*",
            r".*Please upload this file to the index in the xx/xxx/ subdirectory",
        ],
        gen_manifest=[
            r'.*url = "git\+https://github\.com/some_user/repo-name".*',
        ],
        expect_success=True
    )


print("SUCCESS")
