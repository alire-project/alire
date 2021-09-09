"""
Test that a tool built by a dependency is available to a dependent crate
"""

from drivers.alr import run_alr, init_local_crate, alr_with, alr_manifest
from drivers.alr import add_action
from os import chdir
from os.path import join
from shutil import rmtree, which

# We test with a locally pinned dependency, which should make no difference as
# it is a regular release in the eyes of the build process

init_local_crate("root")
init_local_crate("depended", binary=True, enter=False)

alr_with("depended", path="depended")

# Add a pre-build action to the root crate that attempts to run bin/depended
add_action("pre-build", ["depended/bin/depended"])

run_alr("build")

# Now, add the executable to the path in the depended crate, and an action that
# uses only the executable name

# First, ensure that a same-name executable isn't in the environment by some
# bizarre chance
assert which("depended") is None, "Unexpected 'depended' command in PATH"

chdir("depended")
with open(alr_manifest(), "a") as manifest:
    manifest.writelines(["[environment]\n",
                         "PATH.append = '${CRATE_ROOT}/bin'\n"])
chdir("..")
# Clean up a bit just in case
rmtree("alire")
rmtree("depended/alire")

add_action("pre-build", ["depended"])

# Finally verify that a non-existant executable is actually failing. We do this
# using "root" as an intermediate crate, to ensure that "pre-build" is also
# being run for all dependencies and not only the root one.

add_action("pre-build", ["fake_alr_test_exec"])

init_local_crate("root_test")
alr_with("root", path="..")

p = run_alr("build", complain_on_error=False)
assert p.status != 0, "Command should have failed"
assert 'Command ["fake_alr_test_exec"] exited with code 1' in p.out, \
    "Output does not contain expected error, but: " + p.out

print('SUCCESS')
