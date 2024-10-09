"""
Check that local directories and local git repos can be used as indexes.
"""

import os
import re
import shutil
import subprocess

from drivers.alr import init_local_crate, run_alr
from drivers.helpers import init_git_repo, git_branch
from drivers.asserts import assert_eq, assert_match


TEST_ROOT_DIR = os.getcwd()
MY_INDEX_PATH = os.path.join(TEST_ROOT_DIR, 'my_index')


def run(*args, **kwargs):
    sp = subprocess.run(*args, **kwargs)
    sp.check_returncode()
    return sp

def check_index_is_configured(name, url, path):
    # On MacOS, /var/ is a symlink to /private/var/. Since the full pattern
    # below prepends a ".*", "/var/" will match both.
    if path.startswith("/private/var"):
        path = path.removeprefix("/private")
    assert_match(
        rf".*\d+.*{re.escape(name)}.*{re.escape(url)}.*{re.escape(path)}",
        run_alr("index", "--list").out
    )

def check_index_works():
    """
    Use the currently configured index to get, build and run the 'hello' crate,
    asserting success.
    """
    run_alr("get", "hello")
    deploy_dir = run_alr("get", "hello", "--dirname").out.strip()
    os.chdir(deploy_dir)
    assert_eq("Hello, world!\n", run_alr("run").out)
    os.chdir("..")
    shutil.rmtree(deploy_dir)


# Add all necessary paths to the index. We do this now because they need to be
# absolute paths; relative paths don't work for git repo indexes. Note that TOML
# files require backslashes (in windows paths) to be escaped.
my_crates_path = os.path.join(TEST_ROOT_DIR, "my_crates")
hello_manifest_path = os.path.join(
    "my_index", "index", "he", "hello", "hello-1.0.1.toml"
)
libhello_manifest_path = os.path.join(
    "my_index", "index", "li", "libhello", "libhello-1.0.0.toml"
)
hello_path = os.path.join(my_crates_path, "hello").replace("\\", "\\\\")
libhello_path = os.path.join(my_crates_path, "libhello").replace("\\", "\\\\")
with open(hello_manifest_path, "a") as f:
    f.write(f'url = "file:{hello_path}"\n')
with open(libhello_manifest_path, "a") as f:
    f.write(f'url = "file:{libhello_path}"\n')


# Test adding my_index as a simple directory index.
run_alr("index", "--name", "my_index", "--add", "my_index")
check_index_is_configured("my_index", f"file:{MY_INDEX_PATH}", MY_INDEX_PATH)
# Verify this hasn't created an unecessary copy in the alr-config directory
# (only an index.toml file which redirects to the existing copy).
index_copy_path = os.path.join("alr-config", "indexes", "my_index")
assert_eq(["index.toml"], os.listdir(index_copy_path))
# Check that the index can be used to get and build a crate.
check_index_works()
# Clean up for next test case.
run_alr("index", "--del", "my_index")

# Verify same result with --arg=value form.
run_alr("index", "--name=my_index", "--add=my_index")
check_index_is_configured("my_index", f"file:{MY_INDEX_PATH}", MY_INDEX_PATH)
assert_eq(["index.toml"], os.listdir(index_copy_path))
check_index_works()
run_alr("index", "--del", "my_index")

# Verify the same result with a `file:` URL equivalent.
run_alr("index", "--name", "my_index", "--add", "file:my_index")
check_index_is_configured("my_index", f"file:{MY_INDEX_PATH}", MY_INDEX_PATH)
assert_eq(["index.toml"], os.listdir(index_copy_path))
check_index_works()
run_alr("index", "--del", "my_index")

# Verify that a `git+file:` URL fails.
p = run_alr(
    "index", "--name", "my_index", "--add", "git+file:my_index",
    complain_on_error=False
)
unix_pattern = "repository '.*my_index' does not exist"
windows_pattern = "'.*my_index' does not appear to be a git repository"
assert_match(f".*({unix_pattern}|{windows_pattern})", p.out)


# Initialise a normal git repo in the my_index directory.
init_git_repo("my_index")
default_branch = git_branch("my_index")

# Verify that the simple directory cases above still work.
run_alr("index", "--name", "my_index", "--add", "my_index")
check_index_is_configured("my_index", f"file:{MY_INDEX_PATH}", MY_INDEX_PATH)
check_index_works()
run_alr("index", "--del", "my_index")
run_alr("index", "--name", "my_index", "--add", "file:my_index")
check_index_is_configured("my_index", f"file:{MY_INDEX_PATH}", MY_INDEX_PATH)
check_index_works()
run_alr("index", "--del", "my_index")

# Verify that a `git+file:` URL now works, making a clone under alr-config with
# the path (not a `file:` URL) as its remote.
run_alr("index", "--name", "my_index", "--add", "git+file:my_index")
check_index_is_configured(
    "my_index",
    f"git+file:{MY_INDEX_PATH}",
    os.path.join(TEST_ROOT_DIR, "alr-config", "indexes", "my_index", "repo")
)
os.chdir(os.path.join("alr-config", "indexes", "my_index", "repo"))
sp = run(["git", "remote", "show", "origin"], capture_output=True)
assert_match(r".*Fetch URL: (?!(git\+)?file:).*my_index", sp.stdout.decode())
os.chdir(TEST_ROOT_DIR)
check_index_works()
run_alr("index", "--del", "my_index")


# Make a bare repo clone of the index.
run(["git", "clone", "--bare", "my_index", "bare_repo_index"])

# Verify that this clone can't be used as a simple directory index.
p = run_alr(
    "index", "--name", "bare_repo_index", "--add", "bare_repo_index",
    complain_on_error=False
)
assert_match(
    ".*No index version metadata found in .*bare_repo_index",
    p.out
)
p = run_alr(
    "index", "--name", "bare_repo_index", "--add", "file:bare_repo_index",
    complain_on_error=False
)
assert_match(
    ".*No index version metadata found in .*bare_repo_index",
    p.out
)

# Verify that it does work with a `git+file` URL
run_alr(
    "index", "--name", "bare_repo_index", "--add", "git+file:bare_repo_index"
)
check_index_is_configured(
    "bare_repo_index",
    f"git+file:{os.path.join(TEST_ROOT_DIR, 'bare_repo_index')}",
    os.path.join(TEST_ROOT_DIR, "alr-config", "indexes", "bare_repo_index", "repo")
)
os.chdir(os.path.join("alr-config", "indexes", "bare_repo_index", "repo"))
sp = run(["git", "remote", "show", "origin"], capture_output=True)
assert_match(
    r".*Fetch URL: (?!(git\+)?file:).*bare_repo_index",
    sp.stdout.decode()
)
os.chdir(TEST_ROOT_DIR)
check_index_works()
run_alr("index", "--del", "bare_repo_index")


# Add a branch to my_index at the current commit, then change the default branch
# so it is missing the libhello crate.
os.chdir(os.path.join("my_index", "index"))
run(["git", "branch", "other_branch"])
run(["git", "checkout", default_branch])
shutil.rmtree("li")
run(["git", "add", "*"])
run(["git", "commit", "-m", "Remove libhello"])
os.chdir(TEST_ROOT_DIR)

# Verify that `alr index add` adds the checked out branch (so `alr get hello`
# will fail).
run_alr("index", "--name", "my_index", "--add", "git+file:my_index")
check_index_is_configured(
    "my_index",
    f"git+file:{MY_INDEX_PATH}",
    os.path.join(TEST_ROOT_DIR, "alr-config", "indexes", "my_index", "repo")
)
p = run_alr("get", "hello", quiet=False, complain_on_error=False)
assert_match(r".*Could not find a complete solution for hello=1\.0\.1", p.out)
# Verify this doesn't change if the checked out branch is changed subsequent to
# the `alr index add`
os.chdir("my_index")
run(["git", "checkout", "other_branch"])
os.chdir(TEST_ROOT_DIR)
run_alr("index", "--update-all")
p = run_alr("get", "hello", quiet=False, complain_on_error=False)
assert_match(r".*Could not find a complete solution for hello=1\.0\.1", p.out)
run_alr("index", "--del", "my_index")

# Now that other_branch is checked out, verify that `alr index add` adds the
# version with libhello still present, and therefore `alr get hello` succeeds.
run_alr("index", "--name", "my_index", "--add", "git+file:my_index")
check_index_is_configured(
    "my_index",
    f"git+file:{MY_INDEX_PATH}",
    os.path.join(TEST_ROOT_DIR, "alr-config", "indexes", "my_index", "repo")
)
check_index_works()
# Again, check that checkout commands after `alr index add` have no effect
os.chdir("my_index")
run(["git", "checkout", default_branch])
os.chdir(TEST_ROOT_DIR)
run_alr("index", "--update-all")
check_index_works()
run_alr("index", "--del", "my_index")


print("SUCCESS")
