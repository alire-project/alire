"""
Check `alr build` switches to control root profile
"""

import os

from drivers.alr import run_alr, init_local_crate, alr_with, alr_manifest
from drivers.helpers import content_of
from drivers.asserts import assert_match

init_local_crate('lib_1', binary=False, enter=False)
init_local_crate('bin_1', binary=True, enter=True)
run_alr('update')


def check_config(path, profile):
    conf = content_of(path)
    assert_match('.*Build_Profile : Build_Profile_Kind := "%s"' % profile,
                 conf)

    # Extra check for the compiler switches
    profile = profile.lower()
    if profile == 'development':
        assert_match('.*"-Og"', conf)
    elif profile == 'release':
        assert_match('.*"-O3"', conf)
    elif profile == 'validation':
        assert_match('.*"-gnata"', conf)
    else:
        raise Exception("Unknown profile: %s" % profile)


lib1_config = "../lib_1/config/lib_1_config.gpr"
bin_config = "config/bin_1_config.gpr"

mtime = os.path.getmtime(bin_config)


def check_config_changed():
    global mtime
    last_mtime = mtime
    mtime = os.path.getmtime(bin_config)
    assert last_mtime != mtime, "config file timestamp did not change"


def check_config_not_changed():
    global mtime
    last_mtime = mtime
    mtime = os.path.getmtime(bin_config)
    assert last_mtime == mtime, "config file timestamp did unexpectedly change"


# Check default profiles for root and dependency
check_config(bin_config, 'development')

# Check that the project builds
run_alr('build')

# Build in validation mode
run_alr('build', '--validation')
check_config_changed()
check_config(bin_config, 'validation')

# Build in validation mode
run_alr('build', '--development')
check_config_changed()
check_config(bin_config, 'development')

# Build in release mode
run_alr('build', '--release')
check_config_changed()
check_config(bin_config, 'release')

# Alr with does not touch config, that happens at build time
alr_with('lib_1', path='../lib_1')
check_config_not_changed()

# Build with default profile, the config should change and revert to development
run_alr('build')
check_config_changed()
check_config(bin_config, 'development')

print('SUCCESS')
