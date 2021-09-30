"""
Check that alr exec runs commands within Alire environment/context
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

p = run_alr('exec', 'echo', 'test string',
            quiet=False) # -q will hide the output of the exec command

assert_match('test string',
             p.out, flags=re.S)

# exec using -- to separate arguments to the spawned command
p = run_alr('exec', '--', 'sh', '-c',
            'echo "print GPR_PROJECT_PATH from'
            ' alire context:" ${GPR_PROJECT_PATH}',
            quiet=False) # -q will hide the output of the exec command

assert_match('.* GPR_PROJECT_PATH from alire context.*'
             'hello_[0-9\.]*_filesystem.*'
             'libhello_[0-9\.]*_filesystem.*',
             p.out, flags=re.S)

print('SUCCESS')
