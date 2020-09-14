"""
Test a basic auto withing of gpr file.
"""

from glob import glob
import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# Get the "hello" project and enter its directory
run_alr('init', '--bin', 'myhello')
os.chdir(glob('myhello*')[0])

# Get the libhello dependency that should also add a with in update test.gpr
run_alr('with', 'libhello')

# Re-write the source code by using libhello
with open("src/myhello.adb", "w") as text_file:
    text_file.write("""
with libhello;

procedure Myhello is
begin
   libhello.Hello_World;
end Myhello;
""")

# Build it
run_alr('build')

# Run it
p = run_alr('run')
assert_eq('Hello, world!\n', p.out)

# Get a dependency with auto-gpr-with=false
run_alr('with', 'libhello_nogprwith')
# Build again, the build would fail if the project file from libhello_nogprwith
# is wrongly withed.
run_alr('build')

# Get an external dependency
run_alr('with', 'extern')
# Build again, the build would fail if a default project file is added for
# external crates.
run_alr('build')

print('SUCCESS')
