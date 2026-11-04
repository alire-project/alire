"""
Test init command produced artifacts and options
"""

import os.path

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import compare, contents

test_dir = os.getcwd()


def skeleton(root: str, crate: str, tests: bool = True) -> list:
    """
    Sorted paths, under root, of what `alr init --bin crate` creates
    """
    files = [".gitignore",
             "alire",
             "alire.toml",
             "alire/alire.lock",
             "alire/build_hash_inputs",
             "alire/settings.toml",
             "alire/settings.toml.lock",
             "alire/tmp",
             "config",
             f"config/{crate}_config.ads",
             f"config/{crate}_config.gpr",
             f"config/{crate}_config.h",
             "share",
             f"share/{crate}",
             "src",
             f"src/{crate}.adb",
             f"{crate}.gpr"]
    if tests:
        files += ["tests",
                  "tests/.gitignore",
                  "tests/alire.toml",
                  "tests/src",
                  f"tests/src/{crate}_tests-assertions_enabled.adb",
                  f"tests/src/{crate}_tests.ads",
                  f"tests/{crate}_tests.gpr"]
    return sorted(f"{root}/{file}" for file in files)


# Invalid crate name
p = run_alr('init', '--bin', 'invalid-name',
            complain_on_error=False)
assert_match(".*Identifiers must be.*", p.out)

# In the following, config files must already exist so the project is
# immediately loadable by editors. Other artifacts under 'alire/' are created
# during configuration generation.

# Plain init
run_alr('init', '--bin', 'xxx')
compare(contents('xxx'), skeleton('xxx', 'xxx'))

# Plain init, existing empty dir
os.mkdir('aaa')
run_alr('init', '--bin', 'aaa')
compare(contents('aaa'), skeleton('aaa', 'aaa'))

# Init without skeleton
run_alr('init', '--bin', '--no-skel', 'yyy')
compare(contents('yyy'), ['yyy/alire.toml',
                          ])

# Init without tests
run_alr('init', '--bin', '--no-test', 'bbb')
compare(contents('bbb'), skeleton('bbb', 'bbb', tests=False))

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
compare(contents('.'), skeleton('.', 'zzz'))


print('SUCCESS')
