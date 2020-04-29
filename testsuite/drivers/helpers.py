"""
Assorted helpers that are reused by several tests.
"""

import os


# Check a file contains a concrete line
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
            assert False, 'Could not find {} in {}'.format(
                repr(line), filename)


# Return the entries (sorted) under a given folder, both folders and files
def contents(dir):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    return sorted([os.path.join(root, name).replace('\\', '/') for
                   root, dirs, files in os.walk(dir)
                   for name in dirs + files])


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
            assert False, 'Could not find {} in {}'.format(
                repr(line), filename)
