"""
Test init command produced artifacts and options
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import compare, contents

test_dir = os.getcwd()

# Invalid crate name
p = run_alr('init', '--bin', 'invalid-name',
            complain_on_error=False)
assert_match(".*Identifiers must be.*", p.out)

# Plain init
run_alr('init', '--bin', 'xxx')
compare(contents('xxx'), ['xxx/.gitignore',
                          'xxx/alire',
                          'xxx/alire.lock',
                          'xxx/alire.toml',
                          'xxx/src',
                          'xxx/src/xxx.adb',
                          'xxx/xxx.gpr'])

# Plain init, existing empty dir
os.mkdir('aaa')
run_alr('init', '--bin', 'aaa')
compare(contents('aaa'), ['aaa/.gitignore',
                          'aaa/aaa.gpr',
                          'aaa/alire',
                          'aaa/alire.lock',
                          'aaa/alire.toml',
                          'aaa/src',
                          'aaa/src/aaa.adb'])

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire',
                          'yyy/alire.lock',
                          'yyy/alire.toml'])

# Init with existing crate
os.chdir('yyy')
run_alr('init', '--bin', '--no-skel', 'yyy', quiet=False)

# Init in place with existing crate FAIL (we do not overwrite files)
os.chdir('yyy')
p = run_alr('init', '--bin', '--in-place', '--no-skel', 'yyy',
            complain_on_error=False, quiet=False)
assert_match(".*alire.toml already exists.*", p.out)

# Init in place with existing invalid crate will FAIL
with open('alire.toml', 'w') as f:
    f.write("plop")
p = run_alr('init', '--bin', '--in-place', '--no-skel', 'yyy',
            complain_on_error=False)
assert_match(".*Invalid TOML contents.*", p.out)

os.chdir(test_dir)

# Init in place
os.mkdir('zzz')
os.chdir('zzz')
run_alr('init', '--bin', '--in-place', 'zzz')
compare(contents('.'), ['./.gitignore',
                        './alire',
                        './alire.lock',
                        './alire.toml',
                        './src',
                        './src/zzz.adb',
                        './zzz.gpr'])


print('SUCCESS')
