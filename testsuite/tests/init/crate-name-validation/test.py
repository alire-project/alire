'''
Check that the crate name is validated properly
'''

import string

from drivers.alr import run_alr
from drivers.asserts import assert_match

CRATE_TYPES = ['bin', 'lib']
VALID_NAME = f"{string.ascii_lowercase}_{string.digits}."

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

# Leading dot
assert_that(name='.aaa', fails_with='.*Identifiers must not begin with a dot.*')

# Non lowercase ASCII alnum
assert_that(name='aaą', fails_with='.*Identifiers must be lowercase ASCII alphanumerical.*')

# Valid name
for crate_type in CRATE_TYPES:
    run_alr('init', f"--{crate_type}", f"{VALID_NAME}{crate_type}")

print('SUCCESS')
