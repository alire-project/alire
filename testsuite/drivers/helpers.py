"""
Assorted helpers that are reused by several tests.
"""

import os


# Return the entries (sorted) under a given folder, both folders and files
def contents(dir):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    return sorted([os.path.join(root, name).replace('\\', '/') for
                   root, dirs, files in os.walk(dir)
                   for name in dirs + files])


# Return the content of a text file
def content_of(filename):
    out = ''
    with open(filename, 'r') as f:
        for l in f:
            out += l
    return out


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
