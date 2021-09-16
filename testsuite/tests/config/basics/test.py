"""
Check basic features of the alr config command
"""

import os

from glob import glob

from drivers.alr import run_alr

def invalid_key(*args):
    print("Running: alr config %s" % " ".join([item for item in args]))

    p = run_alr('config', *args, complain_on_error=False, quiet=False)

    assert p.status != 0, "command should fail"

    assert "Invalid configration key" in p.out, \
           "Missing error message in: '%s" % p.out

def invalid_builtin(*args):
    print("Running: alr config %s" % " ".join([item for item in args]))

    p = run_alr('config', *args, complain_on_error=False, quiet=False)

    assert p.status != 0, "command should fail"

    assert "Invalid value " in p.out, \
           "Missing error message in: '%s" % p.out

def check_value(key, expected_value, local=True):
    if local:
        get = run_alr('config', '--get', key)
    else:
        get = run_alr('config', '--global', '--get', key)
    assert get.out == expected_value + "\n", "Got '%s'" % get.out

def set_get_unset(key, value, image=None):

    if image is None:
        image = value

    # The key should not be defined
    get1 = run_alr('config', '--global', '--get', key, complain_on_error=False)
    assert get1.status != 0, 'Should not be defined'

    # Define it
    run_alr('config', '--global', '--set', key, value)

    # Check that it is defined
    check_value(key, image, local=False)

    # Unset it
    run_alr('config', '--global', '--unset', key)

    # Check that is it not defined anymore
    get3 = run_alr('config', '--global', '--get', key, complain_on_error=False)
    assert get3.status != 0, 'Should not be defined'

#######################
# invalid config keys #
#######################
invalid_key('--get', '--global', '.test')
invalid_key('--get', '--global', '_test.')
invalid_key('--get', '--global', '_test')
invalid_key('--get', '--global', 'test_')
invalid_key('--get', '--global', 'test..test')
invalid_key('--get', '--global', '@')
invalid_key('--get', '--global', '%')
invalid_key('--get', '--global', '&')
invalid_key('--get', '--global', '#')
invalid_key('--get', '--global', '^')

####################
# invalid builtins #
####################
invalid_builtin('--set', '--global', 'user.github_login', 'This is not a valid login')
invalid_builtin('--set', '--global', 'user.email', '@ This is not @ valid email address@')

###############################
# Global Set, Get, Unset, Get #
###############################
set_get_unset('test.explicit.string', '"str"', image='str')
set_get_unset('test.implicit.string', 'str')
set_get_unset('test.int', '42')
set_get_unset('test.bool', 'true')
set_get_unset('test.float', '0.2', image='2.00000000000000E-01')

################
# Local config #
################

# Check that local operations (default) fail if not in a crate context
p = run_alr('config', '--set', 'test.local', '42', complain_on_error=False)
assert p.status != 0, 'Should fail'

# Get a create to have local context
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Local operation should now work
run_alr('config', '--set', 'test.local', '42')

# Set a local and check its value
run_alr('config', '--set', 'test.override', '"is_local"')
check_value('test.override', 'is_local')

# Set a global and check that the local value is still returned
run_alr('config', '--set', '--global', 'test.override', '"is_global"')
check_value('test.override', 'is_local')

# Leave the crate context (local keys are not available anymore)
os.chdir('..')

# Check that we now see the global value
check_value('test.override', 'is_global', local=False)

# Remove the global key
run_alr('config', '--unset', '--global', 'test.override')

print('SUCCESS')
