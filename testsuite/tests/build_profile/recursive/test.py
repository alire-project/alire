"""
Check build --profiles switch
"""

from drivers.alr import run_alr, init_local_crate, alr_pin, alr_manifest
from drivers.helpers import lines_of, content_of

import os


def check_profile(profile: str, file: str):
    line = f'   Build_Profile : Build_Profile_Kind := "{profile}";\n'
    assert line in lines_of(file), \
        f"Unexpected contents: missing line '{line}' in {file}:\n" + \
        f"{content_of(file)}"


# Prepare a crate with a couple of dependencies

init_local_crate()
init_local_crate("dep1", enter=False)
init_local_crate("dep2", enter=False)

crates = ["xxx", "dep1", "dep2"]

alr_pin("dep1", path="dep1")
alr_pin("dep2", path="dep2")

config_root = os.path.join("config", "xxx_config.gpr")
config_dep1 = os.path.join("dep1", "config", "dep1_config.gpr")
config_dep2 = os.path.join("dep2", "config", "dep2_config.gpr")

config = dict()
config["xxx"] = config_root
config["dep1"] = config_dep1
config["dep2"] = config_dep2

# Verify default profiles in root and dependency
run_alr("update")  # Faster than build
check_profile("development", config_root)
check_profile("release", config_dep1)

# Verify both overrides when all are using defaults
run_alr("build", "--profiles=*=validation")
for crate in crates:
    check_profile("validation", config[crate])

run_alr("build", "--profiles=%=validation")
for crate in crates:
    check_profile("validation", config[crate])

# give one profile and rech both overrides

with open(alr_manifest(), "at") as manifest:
    manifest.write("[build-profiles]\n"
                   "dep1 = 'release'\n")

run_alr("build", "--profiles=*=validation")
for crate in crates:
    check_profile("validation", config[crate])

run_alr("build", "--profiles=%=validation")
check_profile("validation", config_root)
check_profile("release", config_dep1)  # Manifest takes precedence in this case
check_profile("validation", config_dep2)

# override all in manifest and check both overrides

with open(alr_manifest(), "at") as manifest:
    manifest.write("'*' = 'development'\n")

run_alr("build", "--profiles=*=validation")
for crate in crates:
    check_profile("validation", config[crate])

run_alr("build", "--validation", "--profiles=%=validation")
check_profile("validation", config_root)   # Root is governed by command line
check_profile("release", config_dep1)      # Because of explicit crate in manif
check_profile("development", config_dep2)  # Because of wildcard in manifest

# Check individual overriding, which always prevails

run_alr("build", "--profiles=xxx=release,dep1=validation,dep2=validation")
check_profile("release", config_root)
check_profile("validation", config_dep1)
check_profile("validation", config_dep2)

print('SUCCESS')
