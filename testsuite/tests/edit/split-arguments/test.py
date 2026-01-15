"""
Test `alr edit` argument splitting
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_match
from subprocess import run

import os, re, shutil

init_local_crate()

run_alr('settings', '--global', '--set', 'editor.cmd', 'sh -c "echo Argument splitting test"')

p = run_alr('edit', quiet=False)
assert_match('Argument splitting test', p.out, flags=re.S)

print('SUCCESS')
