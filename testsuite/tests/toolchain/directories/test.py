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
config_dir = re.search("config folder:([^\n]*)", p.out).group(1).strip()
config_dir = config_dir.replace("\\", "/")
cache_dir = os.path.join(config_dir, "cache")

# The 'contents` function we use to compare these strings normalizes all paths
# to forward slashes, so we do the same with the config_dir

unk_re = "[0-9]+\.[0-9]+\.[0-9]+_([0-9a-f]{8}|external)"
# Unknown version + (Unknown hash or "external")


def config_path_re(crate):
    return re.escape(f"{config_dir}/cache/dependencies/{crate}_") + unk_re

def check_content(crate):
    paths = contents(cache_dir, crate)
    try:
        assert len(paths) >= 1 and \
            re.search(config_path_re(crate), paths[0]), \
            "Unexpected contents: " + str(paths)
    except:
        print(f"erroneous regex was: {config_path_re(crate)}")
        print(f"erroneous path was: {paths[0]}")
        raise


# First we test manual installation
run_alr("toolchain", "--install", "gnat_native")
check_content("gnat_native")

# Uninstall the compiler and verify absence
run_alr("toolchain", "--uninstall", "gnat_native", quiet=False)
paths = contents(cache_dir, "gnat_native")
assert len(paths) == 0, "Unexpected contents: " + str(paths)

# Require the external compiler and verify the trace appears in install folder
init_local_crate("xxx")
alr_with("gnat_external")
match_solution("gnat_external=.* \(shared\)")
check_content("gnat_external")
paths = contents(".", "gnat_external")
assert len(paths) == 0, "Unexpected contents: " + str(paths)

# Require a cross compiler and verify it is automatically installed
alr_with("gnat_external", delete=True, manual=False)
alr_with("gnat_cross_1")
match_solution("gnat_cross_1=.* \(shared\)")
check_content("gnat_cross_1")

print('SUCCESS')
