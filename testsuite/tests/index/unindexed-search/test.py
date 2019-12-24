"""
Test that an unindexed external is loaded and reported by search
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

p = run_alr('search', 'hello_world', '--external')
assert_match('NAME.*STATUS.*VERSION.*DESCRIPTION.*NOTES.*' +
             'hello_world.*NU.*external.*' +
             'Not yet available through the Alire project\n',
             p.out, flags=re.S)


print('SUCCESS')
