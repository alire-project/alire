'''
Check that the crate name is validated properly
'''

from drivers.alr import run_alr
from drivers.asserts import assert_match

for crate_type in ['--bin', '--lib']:

    # Min length
    p = run_alr('init', crate_type, 'a', complain_on_error=False)
    assert_match('.*Identifier too short.*', p.out)

    # Max length
    p = run_alr('init', crate_type, 'a' * 65, complain_on_error=False)
    assert_match('.*Identifier too long.*', p.out)

    # No leading underscore
    p = run_alr('init', crate_type, '_aaa', complain_on_error=False)
    assert_match('.*Identifiers must not begin with an underscore.*', p.out)

    # No leading dot
    p = run_alr('init', crate_type, '.aaa', complain_on_error=False)
    assert_match('.*Identifiers must not begin with a dot.*', p.out)

    # Lowercase ASCII alnum
    p = run_alr('init', crate_type, 'gęś', complain_on_error=False)
    assert_match('.*Identifiers must be lowercase ASCII alphanumerical.*', p.out)

print('SUCCESS')
