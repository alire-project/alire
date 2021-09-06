"""
Check folders when installing binary compiler crates at a custom location
"""

import os
import re

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from drivers.helpers import contents

install_dir = os.path.join (os.getcwd(), "custom_install").replace("\\", "/")
install_dir_2 = os.path.join (os.getcwd(), "custom_install").replace("\\", "/")


unk_re = "[0-9]+\.[0-9]+\.[0-9]+_[0-9a-f]{8}"  # Unknown version + Unknown hash
def config_path_re(crate):
    return re.escape(f"{install_dir}/{crate}_") + unk_re

def check_content(crate, dir):
    paths = contents(dir, crate)
    try:
        assert len(paths) >= 1 and \
            re.search(config_path_re(crate), paths[0]), \
            "Unexpected contents: " + str(paths)
    except:
        print(f"erroneous regex was: {config_path_re(crate)}")
        print(f"erroneous path was: {paths[0]}")
        raise

# First we test installation of one component
run_alr("toolchain", "--install", "gnat_native", "--install-dir", install_dir)

check_content("gnat_native", install_dir)


# We now test installation of two components
run_alr("toolchain", "--install", "gnat_native", "gnat_cross_1",
        "--install-dir", install_dir_2)

check_content("gnat_native", install_dir_2)
check_content("gnat_cross_1", install_dir_2)

print('SUCCESS')
