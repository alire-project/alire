"""
Test basic crate configuration
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq

import os

# Get and check post fetch action
run_alr('get', 'hello_world')
os.chdir("hello_world_0.1.0_filesystem/")

run_alr('build')
p = run_alr('run')
assert_eq("Ada -> Crate_Version: 1.0.0\n"
          "Ada -> Crate_Name: libcrate_config\n"
          "Ada -> Var_Bool: TRUE\n"
          "Ada -> Var_String: 'Test string.'\n"
          "Ada -> Var_Int: -1\n"
          "Ada -> Var_Real: -1.000000000E+00\n"
          "Ada -> Var_Enum: B\n"
          "C -> Crate_Version: 1.0.0\n"
          "C -> Crate_Name: libcrate_config\n"
          "C -> Var_Bool: 1\n"
          "C -> Var_String: 'Test string.'\n"
          "C -> Var_Int: -1\n"
          "C -> Var_Real: -1.000000\n"
          "C -> Var_Enum: 2\n",
          p.out)

print(p.out)

print('SUCCESS')
