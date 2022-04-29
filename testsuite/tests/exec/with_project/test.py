"""
Check alr exec -P switch support
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

def check(p_switch, expected_out, complain_on_error=True):
    p = run_alr('exec', p_switch, '--', 'echo', '1', '2', '3', '4', '5',
                quiet=False, # -q will hide the output of the exec command
                complain_on_error=complain_on_error)
    assert_match(expected_out, p.out, flags=re.S)

check('-P',      '-P hello.gpr 1 2 3 4 5')
check('-P1',     '-P hello.gpr 1 2 3 4 5')
check('-P2',     '1 -P hello.gpr 2 3 4 5')
check('-P5',     '1 2 3 4 -P hello.gpr 5')
check('-P6',     '1 2 3 4 5 -P hello.gpr')
check('-P7',     '1 2 3 4 5 -P hello.gpr')
check('-P42000', '1 2 3 4 5 -P hello.gpr')

check('-P-1',     '1 2 3 4 5 -P hello.gpr')
check('-P-2',     '1 2 3 4 -P hello.gpr 5')
check('-P-3',     '1 2 3 -P hello.gpr 4 5')
check('-P-5',     '-P hello.gpr 1 2 3 4 5')
check('-P-6',     '-P hello.gpr 1 2 3 4 5')
check('-P-7',     '-P hello.gpr 1 2 3 4 5')
check('-P-42000', '-P hello.gpr 1 2 3 4 5')

check('-P0', '.*Invalid position argument.*', complain_on_error=False)
check('-Ptest', '.*Invalid position argument.*', complain_on_error=False)

print('SUCCESS')
