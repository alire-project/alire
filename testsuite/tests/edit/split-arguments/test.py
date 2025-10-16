"""
Test `alr edit` argument splitting
"""

from drivers.alr import run_alr, CalledProcessError
from drivers.asserts import assert_match
from subprocess import run

import os, re, shutil

alr_path = os.environ['ALR_PATH']

p = run_alr('settings', '--global', '--set', 'editor.cmd', 'sh -c "echo Argument splitting test"')

target = 'noop_1.0.0_filesystem'
p = run_alr('get', 'noop=1.0-default', quiet=True)
os.chdir(target)

p = run([alr_path, "edit"], capture_output=True)
assert_match('Argument splitting test', p.stdout.decode(), flags=re.S)

os.chdir('..')
shutil.rmtree(target)

print('SUCCESS')
