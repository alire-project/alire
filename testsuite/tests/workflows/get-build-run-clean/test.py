"""
Test a basic get-build-run workflow.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import contents


# Get the "hello" project and enter its directory
run_alr('get', 'hello')
os.chdir(glob('hello*')[0])

# Build it
run_alr('build')

# Run it
p = run_alr('run')
assert_eq('Hello, world!\n', p.out)

# Clean it
assert os.listdir('obj')
run_alr('clean')

# NOTE: gprbuild/gprclean 26.0 leaves behind files like:
# obj/development/xxx.adb.stderr
# obj/development/xxx.adb.stdout
# obj/development/xxx.bexch
# obj/development/xxx.cswi
# This is likely a bug in gprclean, so instead of checking that obj/ is empty,
# as we did before, we check that remaining files have one of those extensions.

GPRCLEAN_BUG_EXTENSIONS = {'.stderr', '.stdout', '.bexch', '.cswi'}
spurious = [f for f in contents('obj')
            if os.path.isfile(f)
            and os.path.splitext(f)[1] not in GPRCLEAN_BUG_EXTENSIONS]
assert not spurious, \
    "Unexpected files after clean: " + str(spurious)

print('SUCCESS')
