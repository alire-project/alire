"""
Test a basic init-with-pin-run workflow.
"""

import os

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import check_line_in


# Create a new "xxx" program project
run_alr('init', '--bin', 'xxx')
os.chdir('xxx')

# Make it depend on libhello
session_file = os.path.join('alire', 'xxx.toml')
run_alr('with', 'libhello')
check_line_in(session_file,
              'libhello = "*"  # This line was added by `alr with`')

# Add the corresponding "with" line in xxx.gpr.
#
# TODO: maybe "alr with" should do that automatically?
with open('xxx.gpr', 'r') as f:
    content = f.read()
with open('xxx.gpr', 'w') as f:
    f.write('with "libhello";\n')
    f.write(content)

# Pin the version of libhello and verify pin is there
run_alr('pin', 'libhello')
p = run_alr('pin')
assert_eq('libhello 1.0.0\n', p.out)

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
