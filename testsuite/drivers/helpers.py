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
from typing import Union
from zipfile import ZipFile


def mkcd(dir: str, exist_ok: bool = True):
    """
    Create a directory and cd into it
    """
    os.makedirs(dir, exist_ok=exist_ok)
    os.chdir(dir)


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
        return 'DISTRIBUTION_UNKNOWN'

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

        return 'DISTRIBUTION_UNKNOWN'

    elif on_macos():
        if shutil.which('brew'):
            return 'HOMEBREW'
        elif shutil.which('port'):
            return 'MACPORTS'
        else:
            return 'DISTRIBUTION_UNKNOWN'

    elif on_windows():
        return 'MSYS2'
    else:
        return 'DISTRIBUTION_UNKNOWN'


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


def offset_timestamp(file, seconds):
    """
    Add offset to the modification time of a file
    """
    os.utime(file, (os.path.getatime(file),
                    os.path.getmtime(file) + seconds))


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


def append_to_file(filename : str, lines : []) -> None:
    """
    Append the given lines to a file
    """
    with open(filename, "at") as file:
        file.write("\n".join(lines))


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


def which(exec : str) -> str:
    """
    Return the full path to an executable if it can be found in PATH, or ""
    otherwise. On Windows, ".exe" is automatically appended.
    """
    if on_windows() and not exec.endswith(".exe"):
        return which(f"{exec}.exe")

    return shutil.which(exec)


def exe_name(exec : str) -> str:
    """
    Return the executable name with ".exe" appended on Windows.
    """
    return f"{exec}.exe" if on_windows() else exec


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


MOCK_COMMAND_TEMPLATE = """\
with Ada.Command_Line;
with GNAT.OS_Lib;

procedure Main is
   Num_Args  : constant Integer := Ada.Command_Line.Argument_Count;
   Arg_List  : GNAT.OS_Lib.Argument_List (1 .. Num_Args + 1);
   Exit_Code : Integer;
begin
   --  Set arguments to pass to 'python' (the path to the script, followed by
   --  the arguments to pass thereto).
   Arg_List (1) := new String'("{script_path}");
   for I in 1 .. Num_Args loop
      Arg_List (I + 1) := new String'(Ada.Command_Line.Argument (I));
   end loop;
   --  Run the Python script, passing the output directly to stdout and stderr.
   Exit_Code :=
     GNAT.OS_Lib.Spawn
       (Program_Name => GNAT.OS_Lib.Locate_Exec_On_Path ("python").all,
        Args         => Arg_List);
   --  Imitate the script's exit status.
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Exit_Code));
end Main;
"""

class MockCommand:
    """
    Replace a command with a Python script.

    Can be used as a context manager or via the `enable()` and `disable()`
    methods.

    The mock command is placed under the path `dir`, which is temporarily
    prepended to `PATH`. Changes to `PATH` are overridden in the case of tools
    installed by MSYS2, so a `MockCommand` for such a tool will be ignored
    unless `msys2.install_dir` is also set to an empty directory.

    `dir` should be empty or non-existent, except that it may also be used by
    other instances of `MockCommand` with different `name`s.
    """

    def __init__(self, name: str, script: str, dir: Union[str, os.PathLike]):
        self._name = name
        self._script = script
        self._dir = os.path.realpath(dir)

    def __enter__(self):
        self.enable()

    def __exit__(self, type, value, traceback):
        self.disable()

    def enable(self):
        """
        Enable mocking for the command.
        """
        os.makedirs(self._dir, exist_ok=True)
        # Write the script to the directory
        self._script_dir = os.path.join(self._dir, "scripts")
        self._script_path = os.path.join(self._script_dir, self._name)
        os.makedirs(self._script_dir, exist_ok=True)
        with open(self._script_path, "x") as f:
            f.write(self._script)
        # Only binary executables are consistently recognised on the `PATH` in
        # Windows, so we need to compile a binary wrapper for the script.
        build_dir = os.path.join(self._dir, "build")
        os.makedirs(build_dir)
        with open(os.path.join(build_dir, "main.adb"), "x") as f:
            f.write(MOCK_COMMAND_TEMPLATE.format(script_path=self._script_path))
        run(
            ["gnat", "make", "-q", os.path.join(build_dir, "main.adb")],
            cwd=build_dir
        ).check_returncode()
        # Copy the binary to a directory on PATH
        suffix = ".exe" if on_windows() else ""
        self._bin_dir = os.path.join(self._dir, "path_dir")
        self._bin_path = os.path.join(self._bin_dir, self._name + suffix)
        os.makedirs(self._bin_dir, exist_ok=True)
        shutil.copy(os.path.join(build_dir, "main" + suffix), self._bin_path)
        shutil.rmtree(build_dir)
        os.environ["PATH"] = f'{self._bin_dir}{os.pathsep}{os.environ["PATH"]}'

    def disable(self):
        """
        Disable mocking for the command.
        """
        # Restore PATH
        os.environ["PATH"] = os.environ["PATH"].replace(
            f'{self._bin_dir}{os.pathsep}', '', 1
        )
        # Delete the script and binary
        os.remove(self._bin_path)
        os.remove(self._script_path)



SUBSTITUTION_WRAPPER_TEMPLATE = """\
#! /usr/bin/env python
import subprocess, sys
substitution_dict = {substitution_dict}
# Argument substitutions
args = sys.argv[1:]
for key in substitution_dict:
    args = [arg.replace(key, substitution_dict[key]) for arg in args]
# Run the command
p = subprocess.run([r'{actual_cmd_path}'] + args, capture_output=True)
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

class WrapCommand(MockCommand):
    """
    Wrap the command `name` with string substitutions applied to its arguments
    and output.

    The string substitutions are specified by the dictionary `subs`. Every
    non-overlapping occurrence of each of its keys in a command line argument is
    replaced with its corresponding value before being passed to the command.
    The reverse substitution is applied to the command's output. The
    substitutions are applied in the order in which they appear in `subs`.

    The other arguments are the same as for `MockCommand`.
    """

    def __init__(self, name: str, subs: dict[str:str], dir: Union[str, os.PathLike]):
        wrapper_script = SUBSTITUTION_WRAPPER_TEMPLATE.format(
            substitution_dict=subs,
            actual_cmd_path=shutil.which(name)
        )
        super().__init__(name, wrapper_script, dir)
