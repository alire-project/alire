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

# In the following, config files must already exist so the project is
# immediately loadable by editors. Other artifacts under 'alire/' are created
# during configuration generation.

# Plain init
run_alr('init', '--bin', 'xxx')
compare(contents('xxx'), ['xxx/.gitignore',
                          'xxx/alire',
                          'xxx/alire.toml',
                          'xxx/alire/alire.lock',
                          'xxx/alire/build_hash_inputs',
                          'xxx/alire/settings.toml',
                          'xxx/config',
                          'xxx/config/xxx_config.ads',
                          'xxx/config/xxx_config.gpr',
                          'xxx/config/xxx_config.h',
                          'xxx/share',
                          'xxx/share/xxx',
                          'xxx/src',
                          'xxx/src/xxx.adb',
                          'xxx/xxx.gpr'])

# Plain init, existing empty dir
os.mkdir('aaa')
run_alr('init', '--bin', 'aaa')
compare(contents('aaa'), ['aaa/.gitignore',
                          'aaa/aaa.gpr',
                          'aaa/alire',
                          'aaa/alire.toml',
                          'aaa/alire/alire.lock',
                          'aaa/alire/build_hash_inputs',
                          'aaa/alire/settings.toml',
                          'aaa/config',
                          'aaa/config/aaa_config.ads',
                          'aaa/config/aaa_config.gpr',
                          'aaa/config/aaa_config.h',
                          'aaa/share',
                          'aaa/share/aaa',
                          'aaa/src',
                          'aaa/src/aaa.adb'])

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
                        './alire',
                        './alire.toml',
                        './alire/alire.lock',
                        './alire/build_hash_inputs',
                        './alire/settings.toml',
                        './config',
                        './config/zzz_config.ads',
                        './config/zzz_config.gpr',
                        './config/zzz_config.h',
                        './share',
                        './share/zzz',
                        './src',
                        './src/zzz.adb',
                        './zzz.gpr'])


print('SUCCESS')
