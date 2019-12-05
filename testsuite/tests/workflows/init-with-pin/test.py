"""
Test a basic init-with-pin-run workflow.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq


def check_line_in(filename, line):
    """
    Assert that the `filename` tetx file contains at least one line that
    contains `line`.
    """
    with open(filename, 'r') as f:
        for l in f:
            if l.rstrip() == line:
                break
        else:
            assert False, 'Could not find {} in {}'.format(
                repr(line), filename)


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on libhello
session_file = os.path.join('alire', 'xxx.toml')
run_alr('with', 'libhello')
check_line_in(session_file, 'libhello = "*"')

# Add the corresponding "with" line in xxx.gpr.
#
# TODO: maybe "alr with" should do that automatically?
with open('xxx.gpr', 'r') as f:
    content = f.read()
with open('xxx.gpr', 'w') as f:
    f.write('with "libhello";\n')
    f.write(content)

# Pin the version of libhello
run_alr('pin')
check_line_in(session_file, 'libhello = "=1.0.0"')

# Build and run "xxx"
with open(os.path.join('src', 'xxx.adb'), 'w') as f:
    f.write("""
        with Ada.Text_IO;
        with Libhello;

        procedure XXX is
        begin
           Ada.Text_IO.Put_Line ("This is XXX...");
           Libhello.Hello_World;
        end XXX;
    """)
p = run_alr('run')
assert_eq('This is XXX...\nHello, world!\n', p.out)

print('SUCCESS')
