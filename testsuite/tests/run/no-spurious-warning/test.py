"""
Test that no warning about /etc/os-release is ever emitted
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import compare, contents

from glob import glob

from os import chdir

# Create a new working release
p = run_alr('init', '--bin', 'xxx')
assert_eq('', p.out)

chdir(glob('xxx*')[0])

# Should not emit the warning about missing '/etc/os-release'
p = run_alr('run', '--list',
            quiet=False)
assert_eq(p.out.find('/etc/os-release'), -1)


print('SUCCESS')
