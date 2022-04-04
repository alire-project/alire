"""
Check that crates can define custom switches
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

# Check if we can change the profile of a dependency
manifests = [alr_manifest(), '../lib_1/alire.toml', '../lib_2/alire.toml']

for path in manifests:
    with open(path, "a") as manifest:
        manifest.write('[build-switches]\n')
        manifest.write('"*".optimization = ["-opt-switch", "-opt-switch2"]\n')
        manifest.write('"*".debug_info = ["-debug-info-switch"]\n')
        manifest.write('"*".runtime_checks = ["-runtime-checks-switch"]\n')
        manifest.write('"*".compile_checks = ["-compile-checks-switch"]\n')
        manifest.write('"*".contracts = ["-contracts-switch"]\n')
        manifest.write('"*".style_checks = ["-style-switch", "-style-switch2"]\n')
        manifest.write('"*".ada_version = ["-ada-version"]\n')

run_alr('update')

expected_switches = ['-opt-switch',
                     '-opt-switch2',
                     '-debug-info-switch',
                     '-runtime-checks-switch',
                     '-compile-checks-switch',
                     '-contracts-switch',
                     '-style-switch',
                     '-style-switch2',
                     '-ada-version']

check_config(lib1_config, 'Release', expected_switches)
check_config(lib2_config, 'Release', expected_switches)
check_config(bin_config, 'Development', expected_switches)

print('SUCCESS')
