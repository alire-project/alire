"""
Verify that disabling distro detection works as intended
"""

from drivers.alr import run_alr, distro_is_known, init_local_crate, alr_with, alr_manifest

run_alr("settings", "--global",
        "--set", "distribution.disable_detection", "true")

# Inspect output of `alr version`
assert not distro_is_known(), "Unexpected distro detection"

# Ensure that basic environment can be printed for crates that use
# $DISTRIB_ROOT

init_local_crate()
# Append usage of DISTRIB_ROOT to the manifest
with open(alr_manifest(), "a") as f:
    f.write("""
    [environment]
    PATH.append = "${DISTRIB_ROOT}"
    """)
run_alr("printenv")

print('SUCCESS')
