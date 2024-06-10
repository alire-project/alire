'''
Check that specifying a malformed index name is properly reported
'''

from drivers.alr import run_alr
from drivers.asserts import assert_match

def assert_that(name, fails_with):
    p = run_alr('index', '--add', name, '--name', name, complain_on_error=False, debug=False)
    assert_match(fails_with, p.out)

# < min length
assert_that(name='aa', fails_with='.*Identifier too short.*')

# > max length
assert_that(name='a' * 65, fails_with='.*Identifier too long.*')

# Leading underscore
assert_that(name='_aaa', fails_with='.*Identifiers must not begin with an underscore.*')

# Non lowercase ASCII alnum
assert_that(name='aaą', fails_with='.*Identifiers must be lowercase ASCII alphanumerical.*')

print('SUCCESS')
