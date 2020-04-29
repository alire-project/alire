"""
Test custom actions for `alr test`
"""

from drivers.alr import run_alr
from drivers.helpers import check_line_in

from glob import glob

from os import chdir

p = run_alr('test', '--continue', 'hello')

# Enter logging folder
chdir(glob('hello*')[0])
chdir('alire')

# Check the magic string in the test output log
check_line_in(glob('*.log')[0], 'ABRACADABRA')


print('SUCCESS')
