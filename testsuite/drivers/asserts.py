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
                indent(repr(str(expected))),
                'But got:',
                indent(repr(str(actual)))]
        assert False, '\n'.join(text)


def assert_match(expected_re, actual, label=None, flags=0):
    if not re.match(expected_re, actual, flags=flags):
        text = ['Unexpected {}'.format(label or 'output'),
                'Expecting a match on:',
                indent(repr(expected_re)),
                'But got:',
                indent(repr(actual))]
        assert False, '\n'.join(text)
