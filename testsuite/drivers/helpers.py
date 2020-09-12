"""
Assorted helpers that are reused by several tests.
"""

from subprocess import run
from zipfile import ZipFile

import os


# Return the entries (sorted) under a given folder, both folders and files
def contents(dir):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    return sorted([os.path.join(root, name).replace('\\', '/') for
                   root, dirs, files in os.walk(dir)
                   for name in dirs + files])


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
    assert found == wanted, 'Got:    {}\nWanted: {}'.format(found, wanted)


# Check line appears in file
def check_line_in(filename, line):
    """
    Assert that the `filename` tetx file contains at least one line that
    contains `line`.
    """
    with open(filename, 'r') as f:
        for l in f:
            if l.rstrip() == line:
                break
        else:
            assert False, 'Could not find {} in {}:\n{}'.format(
                repr(line), filename, content_of (filename))


def path_separator():
    return ':' if os.name != 'nt' else ';'


def dir_separator():
    return '/' if os.name != 'nt' else '\\'


# Add a 'with "something";' at the top of a project file
def with_project(file, project):
    with open(file, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write('with "{}";'.format(project) + '\n' + content)


def init_git_repo(path):
    """
    Initialize and commit everything inside a folder, returning the HEAD commit
    """
    start_cwd = os.getcwd()
    os.chdir(path)
    assert run(["git", "init", "."]).returncode == 0
    assert run(["git", "config", "user.email", "alr@testing.com"]) \
        .returncode == 0
    assert run(["git", "config", "user.name", "Alire Testsuite"]) \
        .returncode == 0
    assert run(["git", "add", "."]).returncode == 0
    assert run(["git", "commit", "-m", "repo created"]).returncode == 0
    head_commit = run(["git", "log", "-n1", "--no-abbrev", "--oneline"],
                      capture_output=True).stdout.split()[0]
    os.chdir(start_cwd)
    return head_commit


def zip_dir(path, filename):
    """
    Zip contents of path into filename. Relative paths are preserved.
    """
    with ZipFile(filename, 'w') as zip:
        for dir, subdirs, files in os.walk(path):
            for file in files:
                abs_file = os.path.join(dir, file)
                zip.write(abs_file, abs_file)
