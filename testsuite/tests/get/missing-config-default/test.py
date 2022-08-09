"""
Retrieve a crate with missing defaults for configuration variables
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


# Get the "hello=1.0.0", which has missing defaults for configuration variables
p = run_alr('get', 'hello=1.0.0', complain_on_error=False)

assert p.status != 0, "Get should have failed"

assert_match('.*Configuration variable \'hello.var1\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var2\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var3\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var4\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var5\' not set and has no default value.\n'
             '.*Configuration failed\n',
             p.out)


# Get the "hello=1.0.1", which has missing defaults for configuration variables
# in "libhello".
p = run_alr('get', 'hello=1.0.1', complain_on_error=False)

assert p.status != 0, "Get should have failed"

assert_match('.*Configuration variable \'libhello.var1\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var2\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var3\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var4\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var5\' not set and has no default value.\n'
             '.*Configuration failed\n',
             p.out)

print('SUCCESS')
