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
                          'xxx/tests',
                          'xxx/tests/.gitignore',
                          'xxx/tests/alire.toml',
                          'xxx/tests/common',
                          'xxx/tests/common/xxx_tests.ads',
                          'xxx/tests/src',
                          'xxx/tests/src/xxx_tests-assertions_enabled.adb',
                          'xxx/tests/xxx_tests.gpr',
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
                          'aaa/src/aaa.adb',
                          'aaa/tests',
                          'aaa/tests/.gitignore',
                          'aaa/tests/aaa_tests.gpr',
                          'aaa/tests/alire.toml',
                          'aaa/tests/common',
                          'aaa/tests/common/aaa_tests.ads',
                          'aaa/tests/src',
                          'aaa/tests/src/aaa_tests-assertions_enabled.adb'])

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire.toml',
                          ])

# Init without tests
run_alr('init', '--bin', '--no-test', 'bbb')
compare(contents('bbb'), ['bbb/.gitignore',
                          'bbb/alire',
                          'bbb/alire.toml',
                          'bbb/alire/alire.lock',
                          'bbb/alire/build_hash_inputs',
                          'bbb/alire/settings.toml',
                          'bbb/bbb.gpr',
                          'bbb/config',
                          'bbb/config/bbb_config.ads',
                          'bbb/config/bbb_config.gpr',
                          'bbb/config/bbb_config.h',
                          'bbb/share',
                          'bbb/share/bbb',
                          'bbb/src',
                          'bbb/src/bbb.adb'])

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
                        './tests',
                        './tests/.gitignore',
                        './tests/alire.toml',
                        './tests/common',
                        './tests/common/zzz_tests.ads',
                        './tests/src',
                        './tests/src/zzz_tests-assertions_enabled.adb',
                        './tests/zzz_tests.gpr',
                        './zzz.gpr'])


print('SUCCESS')
