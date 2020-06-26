"""
Test a basic auto withing of gpr file.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Get the "hello" project and enter its directory
run_alr('init', '--bin', 'test')
os.chdir(glob('test*')[0])

# Get the libhello dependency that should also add a with in update test.gpr
run_alr('with', 'libhello')

# Re-write the source code by using libhello
with open("src/test.adb", "w") as text_file:
    text_file.write("""
with Libhello;

procedure Test is
begin
   Libhello.Hello_World;
end Test;
""")

# Build it
run_alr('build')

# Run it
p = run_alr('run')
assert_eq('Hello, world!\n', p.out)

print('SUCCESS')
