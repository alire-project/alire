"""
Check the behavior of the "alr search" command.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq


def format_line(name, status, version, description, notes, matches):
    return '{: <9} {: <7} {: <8} {: <54} {: <6} {: <7}'.format(
        name, status, version, description, notes, matches).rstrip(' ') + '\n'


def format_table(*args):
    lines = [format_line('NAME', 'STATUS', 'VERSION', 'DESCRIPTION', 'NOTES', "MATCHES")]
    for arg in args:
        lines.append(format_line(*arg))
    return ''.join(lines)


# List latest releases crates
p = run_alr('search', '--list')
assert_eq(format_table(
    ('hello', '', '1.0.1', '"Hello, world!" demonstration project', '', ''),
    ('libhello', '', '1.0.0',
     '"Hello, world!" demonstration project support library', '', ''),
), p.out)


# List all releases crates
p = run_alr('search', '--list', '--full')
assert_eq(format_table(
    ('hello', '', '1.0.1', '"Hello, world!" demonstration project', '', ''),
    ('hello', '', '1.0.0', '"Hello, world!" demonstration project', '', ''),
    ('libhello', '', '1.0.0',
     '"Hello, world!" demonstration project support library', '', ''),
), p.out)


# Actually search in the index. First, on crate names
p = run_alr('search', '--crates', 'lib')
assert_eq('libhello  "Hello, world!" demonstration project support library\n', p.out)


# Then on all properties, matching properties name should appear only once
p = run_alr('search', 'libhello-tag')
assert_eq(format_table(
    ('libhello', '', '1.0.0',
     '"Hello, world!" demonstration project support library', '', 'Tag'),
), p.out)

print('SUCCESS')
