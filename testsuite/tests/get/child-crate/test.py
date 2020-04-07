"""
Test retrieval and build for a child release
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import compare, contents

import os
import re

p = run_alr('get', 'libparent_child')
assert_eq('', p.out)

# Check folder contents
os.chdir('libparent_child_1.0.0_child')
compare(contents('.'),
        ['./alire',
         './alire/cache',
         './alire/cache/dependencies',
         './alire/cache/dependencies/libparent_1.0.0_filesystem',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/child',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/child/libchild.gpr',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/child/src',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/child/src/libparent-child.ads',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/libparent.gpr',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/src',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/src/child',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/src/child/libparent-child.ads',
         './alire/cache/dependencies/libparent_1.0.0_filesystem/src/libparent.ads',
         './alire/libparent_child.toml'
         ])

# Verify that the child depends on the parent with the same version
p = run_alr('show')
assert_match('.*Dependencies \(direct\):\n'
             '   libparent=1.0.0\n', p.out, flags=re.S)

# Verify environment contains the project path of the child relative to the parent
p = run_alr('setenv')
sep = re.escape(os.path.sep)
assert_match('.*' + sep + 'libparent_1.0.0_filesystem' + sep + 'child.*', p.out)

# Verify the build succeeds
run_alr('build')


print('SUCCESS')
