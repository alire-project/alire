"""
Check that dynamic dependencies don't affect the use of `alr with`
"""

import os
import re

from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import content_of

manifest = "alire.toml"

run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Manually add a regular and a dynamic dependency
with open(manifest, 'a') as file:
    file.write('[[depends-on]]\n'
               'libhello = "*"\n\n'
               '[[depends-on]]\n'
               '[depends-on."case(os)"."..."]\n'
               'superhello = "*"')

# Check adding a dependency
run_alr('with', 'hello^1')
assert 'hello = "^1"  # This line was added by `alr with`' \
       in content_of(manifest)

# Check removal
run_alr('with', '--del', 'hello')
assert 'hello = "^1"  # This line was added by `alr with`' \
       not in content_of(manifest)

# Check that the dependency that precedes the dynamic expression is removable
run_alr('with', '--del', 'libhello')
assert 'libhello = "*"' not in content_of(manifest)

# Check that empty array entries have been cleaned up
assert content_of(manifest).count('[[depends-on]]') == 1

# Check that removing the dynamic dependency isn't allowed
p = run_alr('with', '--del', 'superhello',
            complain_on_error=False, quiet=False)

assert_match(".*" +
             re.escape("Skipping unsupported conditional dependency: "
                       "(case OS is LINUX => superhello*, "
                       "MACOS => superhello*, WINDOWS => superhello*, "
                       "OS_UNKNOWN => superhello*)") +
             ".*" +
             re.escape("Crate slated for removal is not among"
                       " direct static dependencies: superhello") +
             ".*",
             p.out, flags=re.S)

print('SUCCESS')
