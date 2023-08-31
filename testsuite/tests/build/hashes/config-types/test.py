"""
Check the different config types in the hash inputs
"""

from drivers.alr import alr_with, external_compiler_version, init_local_crate, run_alr
from drivers.builds import find_hash, hash_input
from drivers.asserts import assert_eq
from drivers import builds

run_alr("config", "--set", "--global", "dependencies.shared", "true")

init_local_crate()
alr_with("hello=1.0.1")
builds.sync()

# Chech that the hash inputs contains exactly what we expect it to contain.
# We cannot know the dependency hash in advance as it depends on the compiler.
assert_eq(
    'config:hello.var1=true\n'
    'config:hello.var2=str\n'
    'config:hello.var3=A\n'
    'config:hello.var4=0\n'
    'config:hello.var5=0\n'
    'config:hello.var6=0.00000000000000E+00\n'
    'config:hello.var7=0.00000000000000E+00\n'
    f'dependency:libhello=1.0.0={find_hash("libhello")}\n'
    'external:HELLO_LIBRARY_TYPE=default\n'
    'external:LIBRARY_TYPE=default\n'
    'profile:hello=RELEASE\n'
    f'version:gnat_external={external_compiler_version()}\n',
    hash_input("hello"))

print("SUCCESS")
