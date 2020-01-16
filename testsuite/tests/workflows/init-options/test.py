"""
Test init command produced artifacts and options
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents


# Plain init
run_alr('init', '--bin', 'xxx')
compare(contents('xxx'), ['xxx/alire',
                          'xxx/alire/xxx.toml',
                          'xxx/src',
                          'xxx/src/xxx.adb',
                          'xxx/xxx.gpr'])

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire',
                          'yyy/alire/yyy.toml'])

# Init in place
os.mkdir('zzz')
os.chdir('zzz')
run_alr('init', '--bin', '--in-place', 'zzz')
compare(contents('.'), ['./alire',
                        './alire/zzz.toml'])


print('SUCCESS')
