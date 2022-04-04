"""
Test that reserved configuration variable names are rejected
"""

import os.path

from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_match
from drivers.helpers import content_of

test_dir = os.getcwd()

crate_name = "xxx"
for name in ["crate_version", "Crate_VERsion", "crate_name", "alire_",
             "ada_compiler_switches", "c_compiler_switches"]:
    init_local_crate(name=crate_name)

    with open(alr_manifest(), "at") as manifest:
        manifest.write("[configuration.variables]\n")
        manifest.write('%s = {type = "Boolean", default = false}\n' % name)

    p = run_alr("update", complain_on_error=False)
    assert p.status != 0, "alr should have errored"
    assert_match(".*\n"
                 "ERROR: Configuration variable name '%s.%s' "
                 "is reserved for Alire internal use\n.*" % (crate_name, name.lower()),
                 p.out)

    os.chdir("..")

    # Create a different crate name for the next iteration
    crate_name += "x"

print('SUCCESS')
