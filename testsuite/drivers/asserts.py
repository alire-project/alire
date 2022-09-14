"""
This module provides several helpers to perform user-friendly assertions in
testcases based on Python scripts.
"""

import difflib
import os
import re
import difflib

from drivers.alr import run_alr
from drivers.helpers import contents, lines_of

def indent(text, prefix='  '):
    """
    Return `text` indented using `prefix`.
    """
    return '\n'.join(prefix + repr(line) for line in text.splitlines())


def pretty_diff(expected, actual):
    diff = difflib.unified_diff(expected.splitlines(),
                                actual.splitlines(),
                                fromfile='expected',
                                tofile='actual')

    return "\n".join(diff)


def assert_eq(expected, actual, label=None):
    if expected != actual:
        if isinstance(actual, str) and isinstance(expected, str):
            diff = '\nDiff:\n' + pretty_diff(expected, actual)
        else:
            diff = ''

        text = ['Unexpected {}:'.format(label or 'output'),
                'Expecting:',
                indent(str(expected)),
                'But got:',
                indent(str(actual))]
        assert False, '\n'.join(text) + diff


def assert_contents(dir: str, expected, regex: str = ""):
    """
    Check that entries in dir filtered by regex match the list in contents
    """
    real = contents(dir, regex)
    assert real == expected, \
        f"Wanted contents: {expected}\nBut got: {real}\n"


def assert_match(expected_re, actual, label=None, flags=re.S):
    if not re.match(expected_re, actual, flags=flags):
        text = ['Unexpected {}'.format(label or 'output'),
                'Expecting a match on:',
                indent(expected_re),
                'But got:',
                indent(actual)]
        assert False, '\n'.join(text)


def assert_profile(profile: str, crate: str, root: str = "."):
    """
    Verify that a crate was built with a certain profile
    root: path to where the crate root is
    """
    line = f'   Build_Profile : Build_Profile_Kind := "{profile}";\n'
    file = os.path.join(root, "config", f"{crate}_config.gpr")
    assert line in lines_of(file), \
        f"Unexpected contents: missing line '{line}' in {file}:\n" + \
        f"{content_of(file)}"


def match_solution(regex, escape=False, whole=False):
    "Check whether a regex matches the current solution"
    p = run_alr("with", "--solve")
    wrap = "" if whole else ".*"
    assert_match(wrap +
                 (regex if not escape else re.escape(regex)) +
                 wrap,
                 p.out)
