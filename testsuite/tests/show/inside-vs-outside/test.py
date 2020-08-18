"""
Check that `alr show` shows the same info when used outside/inside working copy
"""

import os

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq


def detach_origin(lines):
    # Return lines without "Origin:", and the origin separately
    # Initially the lines is simply the output, so we split:
    lines = lines.split('\n')
    newlines = []
    origin = ""
    for line in lines:
        if line.startswith("Origin:"):
            origin = line
        else:
            newlines.append(line)
    return '\n'.join(newlines), origin


# Outside run
run1 = run_alr('show', 'libhello')

run_alr('get', 'libhello')
os.chdir(glob('libhello*')[0])

# Inside run
run2 = run_alr('show')

# The origin will change from inside the index to the deployment folder,
# so we have to compare that separately
out1, origin1 = detach_origin(run1.out)
out2, origin2 = detach_origin(run2.out)

assert_eq(out1, out2)

assert origin1.endswith("libhello_1.0.0")             # Original source folder
assert origin2.endswith("libhello_1.0.0_filesystem")  # Created by `alr get`

print('SUCCESS')
