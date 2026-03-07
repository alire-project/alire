"""
Test a basic init-with-pin-run workflow.
"""

import os

from drivers.alr import run_alr, alr_pin, init_local_crate
from drivers.asserts import assert_eq
from drivers.helpers import check_line_in


# Create a new "xxx" program project
init_local_crate(binary=True)

# Make it depend on libhello, it is auto-narrowed down to ^1
session_file = os.path.join('alire.toml')
run_alr('with', 'libhello')
check_line_in(session_file, 'libhello = "^1.0.0"')

# Add the corresponding "with" line in xxx.gpr.
#
# TODO: maybe "alr with" should do that automatically?
with open('xxx.gpr', 'r') as f:
    content = f.read()
with open('xxx.gpr', 'w') as f:
    f.write('with "libhello";\n')
    f.write(content)

# Pin the version of libhello and verify pin is there
alr_pin('libhello', version='1.0')
p = run_alr('pin')
assert_eq('libhello 1.0.0\n', p.out)

# Build and run "xxx"
with open(os.path.join('src', 'xxx.adb'), 'w') as f:
    f.write("""
with Ada.Text_IO;
with libhello;

procedure XXX is
begin
   Ada.Text_IO.Put_Line ("This is XXX...");
   libhello.Hello_World;
end XXX;
""")
p = run_alr('run')
assert_eq('This is XXX...\nHello, world!\n', p.out)

print('SUCCESS')
