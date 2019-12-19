"""
Test init command produced artifacts and options
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

import os

# Get local crate using symlinks
run_alr('--use-symlinks', 'get', 'hello')

assert os.path.islink('hello_1.0.1_filesystem')
assert os.path.islink('hello_1.0.1_filesystem/alire/cache/projects/libhello_1.0.0_filesystem')

assert os.path.exists('hello_1.0.1_filesystem/hello.gpr')
assert os.path.exists('hello_1.0.1_filesystem/alire/cache/projects/libhello_1.0.0_filesystem/libhello.gpr')

print('SUCCESS')
