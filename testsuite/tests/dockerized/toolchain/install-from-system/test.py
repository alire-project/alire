"""
Check that toolchains are correctly installed from the system as needed. Check
also that manually removed tools are reinstalled if still selected. Finally,
check that incomplete toolchain selection is reported whenever the missing tool
is not in PATH.
"""

import json
from shutil import which
import subprocess
from drivers.alr import run_alr, unselect_compiler, unselect_gprbuild
from drivers.asserts import assert_eq, assert_substring

INSTALL_TELLTALE = "The system package 'gprbuild' is about to be installed"

def apt_uninstall(pkg:str, exe:str=""):
    """
    Uninstall a package and verify that an executable, by default named as the
    package, is no longer in PATH. Also remove any packages no longer anchored.
    """
    real_exe = exe if exe != "" else pkg
    subprocess.run(["sudo", "apt-get", "remove", "-y", pkg]).check_returncode()
    subprocess.run(["sudo", "apt-get", "autoremove", "-y"]).check_returncode()
    assert which(real_exe) is None, f"Unexpected executable: {which(real_exe)}"


# Start by selecting the available external tools
run_alr("toolchain", "--select", "gnat_external", "gprbuild")

tools = json.loads(run_alr("--format", "toolchain").out)
# Expected output:
# [
#     {
#         "crate": "gprbuild",
#         "version": "18.0.0",
#         "status": "Default",
#         "notes": "Detected at /usr/bin/gprbuild"
#     },
#     {
#         "crate": "gprbuild",
#         "version": "2021.0.0+0778",
#         "status": "Available",
#         "notes": "Provided by system package: gprbuild"
#     },
#     {
#         "crate": "gnat_external",
#         "version": "10.3.0",
#         "status": "Default",
#         "notes": "Detected at /usr/bin/gnat"
#     }
# ]

# Verify expected toolchain is selected
assert_eq(tools[2]["crate"], "gnat_external")
assert_eq(tools[2]["status"], "Default")
assert_eq(tools[0]["crate"], "gprbuild")
assert_eq(tools[0]["status"], "Default")

# Remove gprbuild from the system and reselect, this should force gprbuild
# installation. We need to --force because the external gnat wants a likewise
# external gprbuild (instead of the system one).
apt_uninstall("gprbuild")
p = run_alr("--force", "toolchain", "--select", "gprbuild")
assert_substring(INSTALL_TELLTALE, p.out)

# Remove gprbuild and try to initialize a crate. This should result in
# reinstallation, as a complete toolchain is needed when the crate solution is
# solved.
apt_uninstall("gprbuild")
p = run_alr("init", "--bin", "crate")
assert_substring(INSTALL_TELLTALE, p.out)

# Unset selected gnat and gprbuild, and re-selecting gprbuild should properly
# reinstall gprbuild. No need to force in this case as no gnat is selected that
# requires a particular gprbuild.
unselect_compiler()
unselect_gprbuild()
apt_uninstall("gprbuild")
p = run_alr("toolchain", "--select", "gprbuild")
assert_substring(INSTALL_TELLTALE, p.out)

# Finally, we remove both gnat and gprbuild and select gprbuild only. This
# should result in a warning that the toolchain is incomplete and no gnat can
# be found. Note that we cannot force a gnat installation, as there are no
# system package definitions for it (because several versions are available
# through system packages) and we don't want to force switches between them.
apt_uninstall("gnat")
apt_uninstall("gprbuild")
p = run_alr("toolchain", "--select", "gprbuild", quiet=False)
assert_substring(INSTALL_TELLTALE, p.out)
assert_substring("""
Warning: Unconfigured tool is not in path, builds will likely fail: gnat
Warning: Please ensure a complete toolchain is available with alr toolchain --select
""", p.out)

print('SUCCESS')
