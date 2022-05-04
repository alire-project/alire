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
                          'xxx/alire.toml',
                          'xxx/share',
                          'xxx/share/xxx',
                          'xxx/src',
                          'xxx/src/xxx.adb',
                          'xxx/workspace.code-workspace',
                          'xxx/xxx.gpr'])

# Plain init, existing empty dir
os.mkdir('aaa')
run_alr('init', '--bin', 'aaa')
compare(contents('aaa'), ['aaa/.gitignore',
                          'aaa/aaa.gpr',
                          'aaa/alire.toml',
                          'aaa/share',
                          'aaa/share/aaa',
                          'aaa/src',
                          'aaa/src/aaa.adb',
                          'aaa/workspace.code-workspace'])

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire.toml',
                          ])

# Init with existing crate
os.chdir('yyy')
run_alr('init', '--bin', '--no-skel', 'yyy', quiet=False)

# Init in place with existing crate FAIL (we do not overwrite files)
os.chdir('yyy')
p = run_alr('init', '--bin', '--in-place', '--no-skel', 'yyy',
            complain_on_error=False, quiet=False)
assert_match(".*alire.toml already exists.*", p.out)

# Init in place with existing invalid crate FAIL (we don't even try to load it)
with open('alire.toml', 'w') as f:
    f.write("plop")
p = run_alr('init', '--bin', '--in-place', '--no-skel', 'yyy',
            complain_on_error=False, quiet=False)
assert_match(".*alire.toml already exists.*", p.out)

os.chdir(test_dir)

# Init in place
os.mkdir('zzz')
os.chdir('zzz')
run_alr('init', '--bin', '--in-place', 'zzz')
compare(contents('.'), ['./.gitignore',
                        './alire.toml',
                        './share',
                        './share/zzz',
                        './src',
                        './src/zzz.adb',
                        './workspace.code-workspace',
                        './zzz.gpr'])


print('SUCCESS')
