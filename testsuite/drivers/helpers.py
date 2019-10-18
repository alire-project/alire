"""
Assorted helpers that are reused by several tests.
"""

import os.path


# Return the entries (sorted) under a given folder, both folders and files
def contents(dir):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    return sorted([os.path.join(root, name) for
                   root, dirs, files in os.walk(dir)
                   for name in dirs + files])


# Assert two values are equal or format the differences
def compare(found, wanted):
    assert found == wanted, 'Got:    {}\nWanted: {}'.format(found, wanted)
