"""
Test custom actions for `alr index-test`
"""

from drivers.alr import run_alr
from drivers.helpers import content_of

from glob import glob

from os import chdir

p = run_alr('index-test', '--continue', 'hello')

# Enter logging folder
chdir(glob('hello*')[0])
chdir('alire')

# Check the magic string in the test output log
log_contents = content_of(glob('*.log')[0])
magic_string_count = log_contents.count("ABRACADABRA")
if magic_string_count == 0:
   assert False, 'action not run'
elif magic_string_count > 1:
   assert False, 'action ran more than once'


print('SUCCESS')
