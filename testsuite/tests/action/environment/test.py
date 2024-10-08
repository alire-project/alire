"""
Verify the proper environment is set up for the action. We create an executable
within a crate that will be run during pre-build. For it to be found, the
proper path must be set in the crate environment. This should work for a
regular build (as it did before) and for a manually triggered action (the bug
being fixed).
"""

import os
from shutil import which
from drivers.alr import run_alr, init_local_crate, add_action, alr_manifest
from drivers.asserts import assert_eq
from drivers.helpers import content_of, on_windows

tool_name = "myalrtestingtool"

# Tool crate to be used during build
init_local_crate(tool_name, enter=False)

# Ensure the tool is not found in path by pure chance!
assert not which(tool_name)

# Build the tool
run_alr("-C", tool_name, "build")

# Create the new crate that uses this tool during pre-build step
init_local_crate()

# Edit its manifest to require it during pre-build
add_action("pre-build", [tool_name + (".exe" if on_windows() else "")])

# Build should fail because the tool is not found in the path
run_alr("build", complain_on_error=False)

# Likewise, stand-alone running of the action should fail
run_alr("action", "pre-build", complain_on_error=False)

# Add the tool location to the crate environment
with open(alr_manifest(), "at") as f:
    f.write(f"""
[environment]
PATH.prepend = "../{tool_name}/bin"
""")

# assert_eq(content_of(alr_manifest()), "XXX")

# Now both build and action should succeed
run_alr("build")
run_alr("action", "pre-build")

print("SUCCESS")
