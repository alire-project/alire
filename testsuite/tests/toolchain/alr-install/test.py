"""
Check folders when installing binary compiler crates at a custom location
through `alr install`
"""

import os

from drivers.alr import run_alr
from drivers.helpers import contents

install_dir_1 = os.path.join (os.getcwd(), "custom_install_").replace("\\", "/") + "1"
install_dir_2 = os.path.join (os.getcwd(), "custom_install_").replace("\\", "/") + "2"


def check_content(file, dir):
    paths = contents(dir)
    assert os.path.isfile(os.path.join(dir, "bin", file)), \
        "Unexpected contents: " + str(paths)


# Ensure destinations exist
os.makedirs(install_dir_1)
os.makedirs(install_dir_2)

# First we test installation of one component
os.chdir(install_dir_1)
run_alr("install", "gnat_native", f"--prefix={install_dir_1}")
check_content("gnat_native.exe", install_dir_1)

# We now test installation of two components
os.chdir(install_dir_2)
run_alr("install", "gnat_native",  f"--prefix={install_dir_2}")
run_alr("install", "gnat_cross_1", f"--prefix={install_dir_2}")

check_content("gnat_native.exe",  install_dir_2)
check_content("gnat_cross_1.exe", install_dir_2)


print('SUCCESS')
