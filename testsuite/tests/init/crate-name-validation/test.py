'''
Check that the crate name is validated properly
'''

import string

from drivers.alr import run_alr
from drivers.asserts import assert_match

CRATE_TYPES = ['bin', 'lib']
VALID_NAME = f"{string.ascii_lowercase}{string.digits}_"

def assert_that(name, fails_with):
    for crate_type in CRATE_TYPES:
        p = run_alr('init', f"--{crate_type}", name, complain_on_error=False)
        assert_match(fails_with, p.out)

# < min length
assert_that(name='aa', fails_with='.*Identifier too short.*')

# > max length
assert_that(name='a' * 65, fails_with='.*Identifier too long.*')

# Leading underscore
assert_that(name='_aaa', fails_with='.*Identifiers must not begin with an underscore.*')

# Non lowercase ASCII alnum
assert_that(name='aaą', fails_with='.*Identifiers must be lowercase ASCII alphanumerical.*')

# No Ada83 keywords
assert_that(name='xor', fails_with='.*Identifier cannot be reserved keyword.*')

# No Ada2022 keywords
assert_that(name='parallel', fails_with='.*Identifier cannot be reserved keyword.*')

# No GPR keywords
assert_that(name='extends', fails_with='.*Identifier cannot be reserved keyword.*')

# Valid name
for crate_type in CRATE_TYPES:
    run_alr('init', f"--{crate_type}", f"{VALID_NAME}{crate_type}")

# Test if Project already exists
run_alr('init', 'foo')
p = run_alr('init', 'foo', complain_on_error=False)
assert_match("ERROR: foo/alire.toml already exists", p.out)

p = run_alr('--chdir=foo', 'init', '--in-place', 'foo', complain_on_error=False)
assert_match("ERROR: alire.toml already exists", p.out)

print('SUCCESS')
