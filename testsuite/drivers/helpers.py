"""
Assorted helpers that are reused by several tests.
"""

import hashlib
import os
import platform
import re
import shutil
import stat
import sys
from subprocess import run
from zipfile import ZipFile


# Return the entries (sorted) under a given folder, both folders and files
# Optionally, return only those matching regex. Uses '/' always as separator.
def contents(dir, regex=""):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    if regex != "":
        matcher = re.compile(regex)
    return sorted([os.path.join(root, name).replace('\\', '/')
                   for root, dirs, files in os.walk(dir)
                   for name in dirs + files
                   if regex == "" or
                   matcher.search(os.path.join(root, name).replace('\\', '/'))
                   ])


# Return the content of a text file as a single string with embedded newlines
def content_of(filename):
    out = ''
    with open(filename, 'r') as f:
        for l in f:
            out += l
    return out


def lines_of(filename):
    """
    Return the contents of a file as an array of lines (with line breaks)
    """
    with open(filename, 'r') as f:
        return f.readlines()


# Assert two values are equal or format the differences
def compare(found, wanted):
    assert found == wanted, '\nGot:\n{}\nWanted:\n{}'.format(found, wanted)


# Check line appears in file
def check_line_in(filename, line):
    """
    Assert that the `filename` text file contains at least one line that
    contains `line`.
    """
    with open(filename, 'r') as f:
        for l in f:
            if l.rstrip() == line:
                break
        else:
            assert False, 'Could not find {} in {}:\n{}'.format(
                repr(line), filename, content_of(filename))


def on_linux():
    return platform.system() == "Linux"


def on_macos():
    return platform.system() == "Darwin"


def on_windows():
    return platform.system() == "Windows"


def distribution():

    if 'ALIRE_TESTSUITE_DISABLE_DISTRO' in os.environ:
        return 'DISTRO_UNKNOWN'

    known_distro = ["debian", "ubuntu", "msys2", "arch", "rhel", "centos", "fedora"]

    if os.path.exists("/etc/os-release"):

        for key in ['id', 'id_like']:
            with open("/etc/os-release") as f:
                for line in f:
                    split = line.strip().split('=')
                    if len(split) == 2:
                        val = split[1].lower().strip('"')
                        print("val = '%s'" % val)
                        if split[0].lower() == key and val in known_distro:
                            return val

        return 'DISTRO_UNKNOWN'

    elif on_macos():
        if shutil.which('brew'):
            return 'HOMEBREW'
        elif shutil.which('port'):
            return 'MACPORTS'
        else:
            return 'DISTRO_UNKNOWN'

    elif on_windows():
        return 'MSYS2'
    else:
        return 'DISTRO_UNKNOWN'


def path_separator():
    return ':' if os.name != 'nt' else ';'


def dir_separator():
    return '/' if os.name != 'nt' else '\\'


def host_architecture():
    host_arch = platform.machine().lower()
    if host_arch == "amd64":
        host_arch = "x86_64"
    elif host_arch == "arm64":
        host_arch = "aarch64"
    return host_arch


def host_os():
    host_os = platform.system().lower()
    if host_os == "darwin":
        host_os = 'macos'
    return host_os

# Add a 'with "something";' at the top of a project file
def with_project(file, project):
    with open(file, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write('with "{}";'.format(project) + '\n' + content)


def git_branch(path="."):
    """
    Return the branch name of the checkout
    """
    start_cwd = os.getcwd()
    os.chdir(path)
    branch = run(["git", "branch"],
                 capture_output=True).stdout.split()[1]
    os.chdir(start_cwd)
    return branch.decode()


def git_head(path="."):
    """
    Return the head commit in a git repo
    """
    start_cwd = os.getcwd()
    os.chdir(path)
    head_commit = run(["git", "log", "-n1", "--no-abbrev", "--oneline"],
                      capture_output=True).stdout.split()[0]
    os.chdir(start_cwd)
    return head_commit.decode()


def git_blast(path):
    """
    Change permissions prior to deletion, as otherwise Windows is uncapable
    of removing git checkouts
    """
    for dirpath, dirnames, filenames in os.walk(path):
        os.chmod(dirpath, stat.S_IRWXU)
        for filename in filenames:
            os.chmod(os.path.join(dirpath, filename), stat.S_IRWXU)
    shutil.rmtree(path)


def git_init_user():
    """
    Initialize git user and email
    """
    run(["git", "config", "user.email", "alr@testing.com"]).check_returncode()
    run(["git", "config", "user.name", "Alire Testsuite"]).check_returncode()


def init_git_repo(path):
    """
    Initialize and commit everything inside a folder, returning the HEAD commit
    """
    start_cwd = os.getcwd()
    os.chdir(path)
    assert run(["git", "init", "."]).returncode == 0
    # You might think to init with --initial-branch=master, but
    # e.g. Centos's git doesn't support this.
    assert run(["git", "checkout", "-b", "master"]).returncode == 0
    git_init_user()

    # Workaround for Windows, where somehow we get undeletable files in temps
    # (we use mode 'a' because a '.gitignore' may already be present):
    with open(".gitignore", "at") as file:
        file.write("*.tmp\n")

    head = commit_all(".")
    os.chdir(start_cwd)
    return head


def commit_all(path):
    """
    Commit all changes in a repo and return the HEAD commit
    """
    start_cwd = os.getcwd()
    os.chdir(path)

    assert run(["git", "add", "."]).returncode == 0
    assert run(["git", "commit", "-m", "repo created"]).returncode == 0
    head_commit = run(["git", "log", "-n1", "--no-abbrev", "--oneline"],
                      capture_output=True).stdout.split()[0]

    os.chdir(start_cwd)
    return head_commit.decode()


def git_commit_file(
    commit_name: str, path: str, content: str, mode: str = "x"
) -> str:
    """
    Write to a file with the specified content and `git commit` it.

    Also returns the commit's hash and attaches the tag `f"tag_{commit_name}"`
    thereto.
    """
    with open(path, mode) as f:
        f.write(content)
    run(["git", "add", path]).check_returncode()
    run(["git", "commit", "-m", f"Commit {commit_name}"]).check_returncode()
    run(["git", "tag", f"tag_{commit_name}"]).check_returncode()
    return git_head()


def zip_dir(path, filename):
    """
    Zip contents of path into filename. Relative paths are preserved.
    """
    with ZipFile(filename, 'w') as zip:
        for dir, subdirs, files in os.walk(path):
            for file in files:
                abs_file = os.path.join(dir, file)
                zip.write(abs_file, abs_file)


def md5sum(file):
    """Return the hex md5 hash of a file"""
    with open(file, "rb") as f:
        file_hash = hashlib.md5()
        while chunk := f.read(8192):
            file_hash.update(chunk)

    return file_hash.hexdigest()


def prepend_to_file(filename : str, lines : []) -> None:
    """
    Prepend the given lines to a file
    """
    old_contents = content_of(filename)
    with open(filename, "wt") as file:
        file.write("\n".join(lines) + "\n" + old_contents)


def replace_in_file(filename : str, old : str, new : str):
    """
    Replace all occurrences of a string in a file
    """
    old_contents = content_of(filename)
    with open(filename, "wt") as file:
        file.write(old_contents.replace(old, new))


def neutral_path(path : str) -> str:
    """
    Return a path with all separators replaced by '/'.
    """
    return path.replace('\\', '/')

class FileLock():
    """
    A filesystem-level lock for tests executed from different threads but
    without shared memory space. Only used on Linux.
    """
    def __init__(self, lock_file_path):
        if not on_linux():
            raise Exception("FileLock is only supported on Linux")

        self.lock_file_path = lock_file_path

    def __enter__(self):
        # Create the lock file if it doesn't exist
        open(self.lock_file_path, 'a').close()

        # Reopen in read mode
        self.lock_file = open(self.lock_file_path, 'r')
        # Acquire the file lock or wait for it
        import fcntl
        fcntl.flock(self.lock_file.fileno(), fcntl.LOCK_EX)

    def __exit__(self, exc_type, exc_val, exc_tb):
        # Release the file lock
        import fcntl
        fcntl.flock(self.lock_file.fileno(), fcntl.LOCK_UN)
        self.lock_file.close()


GIT_WRAPPER_TEMPLATE = """\
#! /usr/bin/env python
import subprocess, sys
substitution_dict = {substitution_dict}
# Argument substitutions
args = sys.argv[1:]
for key in substitution_dict:
    args = [arg.replace(key, substitution_dict[key]) for arg in args]
# Run git
p = subprocess.run(['{actual_git_path}'] + args, capture_output=True)
# Output substitutions
stdout, stderr = p.stdout.decode(), p.stderr.decode()
for key in substitution_dict:
    stdout = stdout.replace(substitution_dict[key], key)
    stderr = stderr.replace(substitution_dict[key], key)
print(stdout, end="")
print(stderr, file=sys.stderr, end="")
# Exit with appropriate error code
sys.exit(p.returncode)
"""

class MockGit:
    """
    NON-WINDOWS-ONLY
    A context manager which mocks the git command with string substitutions.

    The string substitutions are specified by the dictionary substitution_dict.
    Every non-overlapping occurrence of each of its keys in a command line
    argument is replaced with its corresponding value before being passed to
    git. The reverse substitution is applied to git's output. The substitutions
    are applied in the order in which they appear in substitution_dict.

    The mocked version of git will be placed in mock_git_dir, which will be
    temporarily added to PATH.
    """

    def __init__(self, substitution_dict, mock_git_dir):
        self._substitution_dict = substitution_dict
        self._mock_git_dir = mock_git_dir

    def __enter__(self):
        # Mocking on Windows would require git.exe wrapper
        if on_windows():
            print('SKIP: git mocking unavailable on Windows')
            sys.exit(0)

        # Create a wrapper script for git
        wrapper_script = GIT_WRAPPER_TEMPLATE.format(
            substitution_dict=self._substitution_dict,
            actual_git_path=shutil.which("git")
        )
        # Add the directory to PATH
        try:
            os.mkdir(self._mock_git_dir)
        except FileExistsError:
            pass
        os.environ["PATH"] = (
            f'{self._mock_git_dir}{os.pathsep}{os.environ["PATH"]}'
        )
        # Write the script to the directory
        wrapper_descriptor = os.open(
            os.path.join(self._mock_git_dir, "git"),
            flags=(os.O_WRONLY | os.O_CREAT | os.O_EXCL),
            mode=0o764,
        )
        with open(wrapper_descriptor, "w") as f:
            f.write(wrapper_script)

    def __exit__(self, type, value, traceback):
        # Restore PATH
        os.environ["PATH"] = os.environ["PATH"].replace(
            f'{self._mock_git_dir}{os.pathsep}', '', 1
        )
        # Delete the wrapper script
        os.remove(os.path.join(self._mock_git_dir, "git"))
