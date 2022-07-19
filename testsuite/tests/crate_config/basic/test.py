"""
Test basic crate configuration
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import distribution, host_architecture, host_os

import os
import platform

# Get and check post fetch action
run_alr('get', 'hello_world')
os.chdir("hello_world_0.1.0_filesystem/")

expected_host_arch = host_architecture()

expected_host_os = host_os()

expected_host_distro = distribution().lower()

run_alr('build')
p = run_alr('run')
assert_eq("Ada -> Crate_Version: 1.0.0\n"
          "Ada -> Crate_Name: libcrate_config\n"
         f"Ada -> Alire_Host_OS: {expected_host_os}\n"
         f"Ada -> Alire_Host_Arch: {expected_host_arch}\n"
         f"Ada -> Alire_Host_Distro: {expected_host_distro}\n"
          "Ada -> Var_Bool: TRUE\n"
          "Ada -> Var_String: 'Test string.'\n"
          "Ada -> Var_Int'First: -42\n"
          "Ada -> Var_Int'Last:  42\n"
          "Ada -> Var_Int: -1\n"
          "Ada -> Var_Real'First: -4.20000000000000E+01\n"
          "Ada -> Var_Real'Last:  4.20000000000000E+01\n"
          "Ada -> Var_Real: -1.000000000E+00\n"
          "Ada -> Var_Enum_Kind'First: A\n"
          "Ada -> Var_Enum_Kind'Last: C\n"
          "Ada -> Var_Enum: B\n"
         f"Host_Specific -> {expected_host_os} specific\n"
          "C -> Crate_Version: 1.0.0\n"
          "C -> Crate_Name: libcrate_config\n"
         f"C -> Alire_Host_OS: {expected_host_os}\n"
         f"C -> Alire_Host_Arch: {expected_host_arch}\n"
         f"C -> Alire_Host_Distro: {expected_host_distro}\n"
          "C -> VAR_BOOL: 1\n"
          "C -> VAR_STRING: 'Test string.'\n"
          "C -> VAR_INT_FIRST: -42\n"
          "C -> VAR_INT_LAST: 42\n"
          "C -> VAR_INT: -1\n"
          "C -> VAR_REAL_FIRST: -42.000000\n"
          "C -> VAR_REAL_LAST: 42.000000\n"
          "C -> VAR_REAL: -1.000000\n"
          "C -> VAR_ENUM_A: 1\n"
          "C -> VAR_ENUM_B: 2\n"
          "C -> VAR_ENUM_C: 3\n"
          "C -> VAR_ENUM: 2\n",
          p.out)

print(p.out)

print('SUCCESS')
