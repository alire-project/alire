"""
Check switches for default profiles
"""

from drivers.alr import run_alr, init_local_crate, alr_with, alr_manifest
from drivers.helpers import content_of
from drivers.asserts import assert_match

init_local_crate('lib_1', binary=False, enter=False)
init_local_crate('lib_2', binary=False, enter=False)
init_local_crate('bin_1', binary=True, enter=True)
alr_with('lib_1', path='../lib_1')
alr_with('lib_2', path='../lib_2')
run_alr('update')


def check_config(path, profile, expected_switches=[]):
    conf = content_of(path)
    assert_match('.*Build_Profile : Build_Profile_Kind := "%s"' % profile,
                 conf)

    for sw in expected_switches:
        assert_match('.*"%s"' % sw,conf)


lib1_config = "../lib_1/config/lib_1_config.gpr"
lib2_config = "../lib_2/config/lib_2_config.gpr"
bin_config = "config/bin_1_config.gpr"

# Check default profiles for root and dependency
check_config(lib1_config, 'release', ['-O3', '-gnatn', '-gnatW8'])
check_config(lib2_config, 'release', ['-O3', '-gnatn', '-gnatW8'])
check_config(bin_config, 'development', ['-Og', '-g', '-gnatwa', '-gnaty3', '-gnatW8'])

# Check if we can change the profile of a dependency
with open(alr_manifest(), "a") as manifest:
    manifest.write('[build-profiles]\n')
    manifest.write('lib_1 = "development"\n')
run_alr('update')
check_config(lib1_config, 'development', ['-Og'])

# Check wildcard profile setting
with open(alr_manifest(), "a") as manifest:
    manifest.write('"*" = "validation"\n')
run_alr('update')
check_config(lib1_config, 'development', ['-Og'])
check_config(lib2_config, 'validation', ['-gnatwe'])
check_config(bin_config, 'validation', ['-gnatwe'])

# Check that the project builds
run_alr('build')

print('SUCCESS')
