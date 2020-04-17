"""
Test init command with a hierarchical crate name
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents


# Init new crate
run_alr('init', '--bin', 'xxx.yyy.zzz')
compare(contents('xxx.yyy.zzz'),
        ['xxx.yyy.zzz/alire',
         'xxx.yyy.zzz/alire/xxx-yyy-zzz.toml',
         'xxx.yyy.zzz/src',
         'xxx.yyy.zzz/src/xxx_yyy_zzz.adb',
         'xxx.yyy.zzz/xxx_yyy_zzz.gpr'])

# Check that it builds
os.chdir('xxx.yyy.zzz')
run_alr('build')


print('SUCCESS')
