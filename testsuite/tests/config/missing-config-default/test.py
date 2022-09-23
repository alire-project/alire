"""
Retrieve a crate with missing defaults for configuration variables
"""

import os

from drivers.alr import run_alr, alr_manifest, alr_touch_manifest, init_local_crate
from drivers.asserts import assert_match
from glob import glob

# get/with must succeed, but showing warnings
# build must fail


# Get the "hello=1.0.0", which has missing defaults for configuration variables
p = run_alr('get', '--build', 'hello=1.0.0', complain_on_error=False)

assert p.status != 0, "Get with build should have failed"

assert_match('.*Configuration variables without a default remain unset\n',
             p.out)


# Get the "hello=1.0.1", which has missing defaults for configuration variables
# in "libhello".
p = run_alr('get', '--build', 'hello=1.0.1', complain_on_error=False)

assert p.status != 0, "Get with build should have failed"

assert_match('.*Configuration variables without a default remain unset\n',
             p.out)

# Get without build must succeed with the missing values as warnings
p = run_alr("get", "hello=1.0.0", quiet=False)

assert_match('.*Configuration variable \'hello.var1\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var2\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var3\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var4\' not set and has no default value.\n'
             '.*Configuration variable \'hello.var5\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var1\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var2\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var3\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var4\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var5\' not set and has no default value.\n'
             '.*Skipping generation of incomplete configuration files for crate hello\n'
             '.*Skipping generation of incomplete configuration files for crate libhello\n'
             '\n'
             'hello=1.0.0 successfully retrieved.',
             p.out)

# Attempting to build now should fail
os.chdir(glob("hello_1.0.0_*")[0])
p = run_alr("build", complain_on_error=False)
assert p.status != 0, "Build should have failed"
assert_match('.*Configuration variables without a default remain unset\n',
             p.out)

# Verify that providing the values in the manifest allows the build to work
with open(alr_manifest(), "at") as manifest:
    manifest.write("""
[configuration.values]
hello.var1 = true
hello.var2 = "string"
hello.var3 = "A"
hello.var4 = 1
hello.var5 = 1.0
""")

# Ensure the edition is noticed
alr_touch_manifest()

# Configuration is still incomplete as the libhello vars are unset
p = run_alr("build", complain_on_error=False)
assert p.status != 0, "Build should have failed"
assert_match('.*Configuration variables without a default remain unset\n',
             p.out)

# Provide values for libhello
with open(alr_manifest(), "at") as manifest:
    manifest.write("""
libhello.var1 = true
libhello.var2 = "string"
libhello.var3 = "A"
libhello.var4 = 1
libhello.var5 = 1.0
""")

# Ensure the edition is noticed
alr_touch_manifest()

# It should succeed now (it fails, because there are no actual project/sources)
p = run_alr("build", complain_on_error=False)
assert p.status != 0, "Build should have failed"
assert_match('.*gprbuild(.exe)?: project file .*hello.gpr" not found.*', p.out)


# New test from scratch, one can "with" a library with values without defaults
init_local_crate()
run_alr("with", "libhello")  # Shouldn't fail
p = run_alr("build", complain_on_error=False, quiet=False)
assert p.status != 0, "Build should have failed"
assert_match('.*Configuration variable \'libhello.var1\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var2\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var3\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var4\' not set and has no default value.\n'
             '.*Configuration variable \'libhello.var5\' not set and has no default value.\n'
             '.*Configuration variables without a default remain unset\n',
             p.out)

print('SUCCESS')
