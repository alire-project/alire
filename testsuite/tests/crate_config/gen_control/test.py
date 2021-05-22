"""
Test basic crate configuration
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import contents

import os

# Get and check post fetch action
run_alr('get', 'hello_world', quiet=False, debug=True)
os.chdir("hello_world_0.1.0_filesystem/")

run_alr('build', quiet=False, debug=True)
p = run_alr('run')
assert_eq("Ada -> Var_Bool: TRUE\n"
          "Ada -> Var_String: 'Test string.'\n"
          "Ada -> Var_Int: -1\n"
          "Ada -> Var_Real: -1.000000000E+00\n"
          "Ada -> Var_Enum: B\n",
          p.out)

assert not os.path.isdir('config'), "config should not be created for root project"

for top, dirs, files in os.walk('./'):
    for nm in files:
        path = os.path.join(top, nm)
        if 'plop_config' in path:
            assert not path.endswith('.h'), "C header should not be generated"
            assert not path.endswith('.gpr'), "GPR should not be generated"

print(p.out)

print('SUCCESS')
