"""
Check created folders are where expected when installing binary compiler crates
"""

import os
import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from drivers.helpers import contents

# Identify config location
p = run_alr("version")
config_dir = re.search("config folder is ([^\n.]*)", p.out).group(1)
unk_re = "[0-9]+\.[0-9]+\.[0-9]+_[0-9a-f]{8}"  # Unknown version + Unknown hash


def config_path_re(crate):
    return f"{config_dir}/cache/dependencies/{crate}_" + unk_re


# First we test manual installation
run_alr("toolchain", "--install", "gnat_native")
# This next call returns all paths related to the installed compiler
paths = contents(config_dir, "gnat_native")
assert len(paths) >= 1 and \
    re.search(config_path_re("gnat_native"), paths[0]), \
    "Unexpected contents: " + str(paths)

# Uninstall the compiler and verify absence
run_alr("toolchain", "--uninstall", "gnat_native")
paths = contents(config_dir, "gnat_native")
assert len(paths) == 0, "Unexpected contents: " + str(paths)

# Require the external compiler and verify no trace appears in install folder
# nor in local folder
init_local_crate("xxx")
alr_with("gnat_external")
match_solution("gnat_external=.* \(installed\)")
paths = contents(config_dir, "gnat_external")
assert len(paths) == 0, "Unexpected contents: " + str(paths)
paths = contents(".", "gnat_external")
assert len(paths) == 0, "Unexpected contents: " + str(paths)

# Require a cross compiler and verify it is automatically installed
alr_with("gnat_external", delete=True, manual=False)
alr_with("gnat_cross_1")
match_solution("gnat_cross_1=.* \(installed\)")
paths = contents(config_dir, "gnat_cross_1")
assert len(paths) >= 1 and \
    re.search(config_path_re("gnat_cross_1"), paths[0]), \
    "Unexpected contents: " + str(paths)

print('SUCCESS')
