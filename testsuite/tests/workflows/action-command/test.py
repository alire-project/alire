"""
Test invalid command TOML type
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import compare, contents

p = run_alr('get', 'hello_world')

expected = 'hello_world_0.1.0_filesystem/test_post_fetch'
directory = 'hello_world_0.1.0_filesystem/'

if not expected in contents(directory):
    assert False, "%s not found in %s\n Got: %s" % \
           (expected, directory, str(contents(directory)))
else:
   print('SUCCESS')
