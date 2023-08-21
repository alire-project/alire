"""
Assorted helpers that are reused by several tests.
"""

import hashlib
import os
import platform
import re
import shutil
import stat
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

    if os.environ.get('ALIRE_DISABLE_DISTRO') == 'true':
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
    assert run(["git", "config", "user.email", "alr@testing.com"]) \
        .returncode == 0
    assert run(["git", "config", "user.name", "Alire Testsuite"]) \
        .returncode == 0

    # Workaround for Windows, where somehow we get undeletable files in temps:
    with open(".gitignore", "wt") as file:
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


def replace_in_file(filename : str, old : str, new : str):
    """
    Replace all occurrences of a string in a file
    """
    old_contents = content_of(filename)
    with open(filename, "wt") as file:
        file.write(old_contents.replace(old, new))


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
