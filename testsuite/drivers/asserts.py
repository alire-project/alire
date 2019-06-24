"""
This module provides several helpers to perform user-friendly assertions in
testcases based on Python scripts.
"""


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
