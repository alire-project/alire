"""
Test init command produced artifacts and options
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Return the entries (sorted) under a given folder, both folders and files
def contents(dir):
    assert os.path.exists(dir), "Bad path for enumeration: {}".format(dir)
    return sorted([os.path.join(root, name) for
                   root, dirs, files in os.walk(dir)
                   for name in dirs + files])


def compare(found, wanted):
    assert found == wanted, 'Got:    {}\nWanted: {}'.format(found, wanted)


# Plain init
run_alr('init', '--bin', 'xxx')
compare(contents('xxx'), ['xxx/alire',
                          'xxx/alire/alr_build.gpr',
                          'xxx/alire/xxx.toml',
                          'xxx/src',
                          'xxx/src/xxx.adb',
                          'xxx/xxx.gpr'])

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire',
                          'yyy/alire/alr_build.gpr',
                          'yyy/alire/yyy.toml'])

# Init in place
os.mkdir('zzz')
os.chdir('zzz')
run_alr('init', '--bin', '--in-place', 'zzz')
compare(contents('.'), ['./alire',
                        './alire/alr_build.gpr',
                        './alire/zzz.toml'])


print('SUCCESS')
