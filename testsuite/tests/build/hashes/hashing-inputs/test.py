"""
Test that the inputs to the hashing properly reflect the build profile and
other inputs.
"""

import os
import shutil
from drivers.alr import alr_with, external_compiler_version, init_local_crate, run_alr
from drivers.builds import find_hash, hash_input
from drivers.asserts import assert_eq, assert_match
from drivers import builds
from drivers.helpers import content_of

init_local_crate()
alr_with("hello")

# Build the crate in default mode, so dependencies are in RELEASE mode
run_alr("build")
hash1 = find_hash("libhello")
assert_match(".*profile:libhello=RELEASE.*",
             hash_input("libhello"))

# Build with dependencies in VALIDATION mode
# Clean up first because find_hash() will fail if there are multiple builds
shutil.rmtree(builds.path())
run_alr("build", "--profiles=*=validation")
hash2 = find_hash("libhello")
assert_match(".*profile:libhello=VALIDATION.*",
             hash_input("libhello"))

# Check that the hashes are different
assert hash1 != hash2, "Hashes should be different"

# Check that the hash inputs contains exactly what we expect it to contain.
# This includes environment variables, GPR externals set or observed, build
# profile, compiler version.

assert_eq(
    'config:libhello.var1=false\n'           # crate config var (set by hello)
    'environment:TEST_ENV=myenv\n'           # plain env var set
    'external:LIBHELLO_LIBRARY_TYPE=default\n'
    'external:LIBRARY_TYPE=default\n'        # hardcoded undeclared GPR external
    'external:TEST_FREEFORM_UNSET=default\n' # declared unset GPR external
    'external:TEST_GPR_EXTERNAL=gpr_ext_B\n' # declared set GPR external
    'external:TEST_UNDECLARED=used_by_another_crate\n' # modified GPR external
    'profile:libhello=VALIDATION\n'          # build profile
    'switches:libhello=-O3,-fdata-sections,-ffunction-sections,-g,-gnatVa,'
    '-gnatW8,-gnata,-gnatn,-gnato,-gnatw.X,-gnatwa,-gnatwe\n'
    f'version:gnat_external={external_compiler_version()}\n',
                                             # compiler version
    hash_input("libhello"))

# Check the hash inputs of a crate with dependencies itself (hello -> libhello).
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
    'profile:hello=VALIDATION\n'
    'switches:hello=-O3,-fdata-sections,-ffunction-sections,-g,-gnatVa,'
    '-gnatW8,-gnata,-gnatn,-gnato,-gnatw.X,-gnatwa,-gnatwe\n'
    f'version:gnat_external={external_compiler_version()}\n',
    hash_input("hello"))

# Bonus: check the hash inputs for the root crate, used for config regeneration
# Using find_hash here ensures that the hash in the dir name matches the one in
# the inputs of a different crate that depends on it.
assert_eq(
    f'dependency:hello=1.0.1={find_hash("hello")}\n'
    'external:LIBRARY_TYPE=default\n'
    'external:XXX_LIBRARY_TYPE=default\n'
    'profile:xxx=VALIDATION\n'
    'switches:xxx=-O3,-fdata-sections,-ffunction-sections,-g,-gnatVa,'
    '-gnatW8,-gnata,-gnatn,-gnato,-gnatw.X,-gnatwa,-gnatwe\n',
    content_of(os.path.join("alire", "build_hash_inputs"))
)

print("SUCCESS")
