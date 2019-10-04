# -*- coding: utf-8 -*-
"""
Test folder hashing internals (order, items, contents)

The following folder structure is being hashed in this test:

./a		# file
./b		# dir
./b/x		# file
./b/y		# dir
./b/y/p		# file
./b/y/q		# file
./b/z		# file
./á		# utf8-encoded file
./c		# file

"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Get the release and check hashing log messages
p = run_alr('get', '-vv', 'libfoo',
            complain_on_error=True, quiet=False)
assert_match(
    '.*Hashing: entry: \./'
    '.*Hashing: - kind: d'
    '.*Hashing: - count: 4'
    '.*Hashing: entry: \./a'
    '.*Hashing: - name: /a \(2 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 2'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./b'
    '.*Hashing: - name: /b \(2 bytes\)'
    '.*Hashing: - kind: d'
    '.*Hashing: - count: 3'
    '.*Hashing: entry: \./b/x'
    '.*Hashing: - name: /b/x \(4 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 0'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./b/y'
    '.*Hashing: - name: /b/y \(4 bytes\)'
    '.*Hashing: - kind: d'
    '.*Hashing: - count: 2'
    '.*Hashing: entry: \./b/y/p'
    '.*Hashing: - name: /b/y/p \(6 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 0'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./b/y/q'
    '.*Hashing: - name: /b/y/q \(6 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 0'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./b/z'
    '.*Hashing: - name: /b/z \(4 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 0'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./c'
    '.*Hashing: - name: /c \(2 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 0'
    '.*Hashing: - contents \(binary\)'
    '.*Hashing: entry: \./á'
    '.*Hashing: - name: /á \(3 bytes\)'
    '.*Hashing: - kind: f'
    '.*Hashing: - size: 3'
    '.*Hashing: - contents \(binary\)'''
    '.*',
    p.out, flags=re.S)

print('SUCCESS')
