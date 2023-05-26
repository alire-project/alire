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
from typing import List

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
    Check that entries in dir filtered by regex match the list in expected
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
    line = f'   Build_Profile : Build_Profile_Kind := "{profile.lower()}";\n'
    file = os.path.join(root, "config", f"{crate}_config.gpr")
    assert line in lines_of(file), \
        f"Unexpected contents: missing line '{line}' in {file}:\n" + \
        f"{lines_of(file)}"


def match_solution(regex, escape=False, whole=False):
    "Check whether a regex matches the current solution"
    p = run_alr("with", "--solve")
    wrap = "" if whole else ".*"
    assert_match(wrap +
                 (regex if not escape else re.escape(regex)) +
                 wrap,
                 p.out)

def assert_installed(prefix : str, milestones : List[str]):
    """
    Check that installed releases match those given in milestones
    """

    p = run_alr("install", "--info", f"--prefix={prefix}", quiet=False)

    milestones.sort()

    assert_eq(f"Installation prefix found at {prefix}\n"
              "Contents:\n"
              "   " + "\n   ".join(milestones) + "\n",
              p.out)


def assert_file_exists(path : str):
    """
    Check that a file exists
    """
    assert os.path.exists(path), f"Missing expected file {path}"


def assert_in_file(path : str, expected : str):
    """
    Check that a file contains a string
    """
    with open(path, "r") as f:
        contents = f.read()
    assert expected in contents, \
        f"Missing expected string '{expected}' in file {path}:\n{contents}"


def match_deploy_dir(crate : str, path_fragment : str):
    """
    Check that a deployment directory for a crate matches a regex. The path
    fragment must be anything between the variable name and the crate name in
    the output of printenv, e.g.: MAKE_ALIRE_PREFIX=<matchable part><crate name>
    """
    p = run_alr("printenv")
    assert_match(f".*[: ]{crate.upper()}_ALIRE_PREFIX=[^\\n]*{path_fragment}[^\\n]*{crate}_.*",
                 p.out)