"""
This module provides several helpers to perform user-friendly assertions in
testcases based on Python scripts.
"""

import re


def indent(text, prefix='  '):
    """
    Return `text` indented using `prefix`.
    """
    return '\n'.join(prefix + repr(line) for line in text.splitlines())


def assert_eq(expected, actual, label=None):
    if expected != actual:
        text = ['Unexpected {}:'.format(label or 'output'),
                'Expecting:',
                indent(str(expected)),
                'But got:',
                indent(str(actual))]
        assert False, '\n'.join(text)


def assert_match(expected_re, actual, label=None):
    if not re.match(expected_re, actual):
        text = ['Unexpected {}'.format(label or 'output'),
                'Expecting a match on:',
                indent(expected_re),
                'But got:',
                indent(actual)]
        assert False, '\n'.join(text)
