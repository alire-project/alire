'''
Check that the crate name is validated properly
'''

from drivers.alr import run_alr
from drivers.asserts import assert_match

def assert_that(name, fails_with):
    for crate_type in ['--bin', '--lib']:
        p = run_alr('init', crate_type, name, complain_on_error=False)
        assert_match(fails_with, p.out)

# Min length
assert_that(name='a', fails_with='.*Identifier too short.*')

# Max length
assert_that(name='a' * 65, fails_with='.*Identifier too long.*')

# No leading underscore
assert_that(name='_aaa', fails_with='.*Identifiers must not begin with an underscore.*')

# No leading dot
assert_that(name='.aaa', fails_with='.*Identifiers must not begin with a dot.*')

# Lowercase ASCII alnum
assert_that(name='aaą', fails_with='.*Identifiers must be lowercase ASCII alphanumerical.*')

print('SUCCESS')
