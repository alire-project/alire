"""
Test basic crate configuration
"""

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.helpers import distribution

import os
import platform

# Get and check post fetch action
run_alr('get', 'hello_world')
os.chdir("hello_world_0.1.0_filesystem/")

expected_host_arch = platform.machine().lower()
if expected_host_arch == "amd64":
    expected_host_arch = "x86_64"

expected_host_os = platform.system().lower()
if expected_host_os == "darwin":
    expected_host_os = 'macos'

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
          "Ada -> Var_Int: -1\n"
          "Ada -> Var_Real: -1.000000000E+00\n"
          "Ada -> Var_Enum: B\n"
         f"Host_Specific -> {expected_host_os} specific\n"
          "C -> Crate_Version: 1.0.0\n"
          "C -> Crate_Name: libcrate_config\n"
         f"C -> Alire_Host_OS: {expected_host_os}\n"
         f"C -> Alire_Host_Arch: {expected_host_arch}\n"
         f"C -> Alire_Host_Distro: {expected_host_distro}\n"
          "C -> Var_Bool: 1\n"
          "C -> Var_String: 'Test string.'\n"
          "C -> Var_Int: -1\n"
          "C -> Var_Real: -1.000000\n"
          "C -> Var_Enum: 2\n",
          p.out)

print(p.out)

print('SUCCESS')
