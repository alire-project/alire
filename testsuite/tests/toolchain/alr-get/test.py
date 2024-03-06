"""
Check folders when installing binary compiler crates at a custom location
through `alr get`
"""

import os
import re
import shutil

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq, assert_match, match_solution
from drivers.helpers import contents

install_dir   = os.path.join (os.getcwd(), "custom_install").replace("\\", "/")
install_dir_2 = os.path.join (os.getcwd(), "custom_install").replace("\\", "/") + "2"


unk_re = "[0-9]+\.[0-9]+\.[0-9]+_[0-9a-f]{8}"  # Unknown version + Unknown hash
def config_path_re(crate, dir):
    return re.escape(f"{dir}/{crate}_") + unk_re

def check_content(crate, dir):
    paths = contents(dir, crate)
    try:
        assert len(paths) >= 1 and \
            re.search(config_path_re(crate, dir), paths[0]), \
            "Unexpected contents: " + str(paths)
    except:
        print(f"erroneous regex was: {config_path_re(crate)}")
        print(f"erroneous path was: {paths[0]}")
        raise

# Ensure destinations exist
os.makedirs(install_dir)
os.makedirs(install_dir_2)

# First we test installation of one component
os.chdir(install_dir)
run_alr("get", "gnat_native")

check_content("gnat_native", install_dir)


# We now test installation of two components
os.chdir(install_dir_2)
run_alr("get", "gnat_native")
run_alr("get", "gnat_cross_1")

check_content("gnat_native", install_dir_2)
check_content("gnat_cross_1", install_dir_2)


print('SUCCESS')
