"""
This module provides several helpers to perform user-friendly assertions in
testcases based on Python scripts.
"""

import re
import difflib


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


def assert_match(expected_re, actual, label=None, flags=re.S):
    if not re.match(expected_re, actual, flags=flags):
        text = ['Unexpected {}'.format(label or 'output'),
                'Expecting a match on:',
                indent(expected_re),
                'But got:',
                indent(actual)]
        assert False, '\n'.join(text)
