"""
Test `alr run` without specific executable in various scenarios
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

import os, re, shutil

target = 'noop_1.0.0_filesystem'


def check_run(release, match=""):
    p = run_alr('get', release, quiet=True)
    # Enter working copy and try to run default executable
    os.chdir(target)
    p = run_alr('run', quiet=not match)
    # Check output when pattern was given:
    if match:
        assert_match(match, p.out, flags=re.S)
    # Otherwise run worked as expected

    os.chdir('..')
    shutil.rmtree(target)


# Check success using a single, undeclared default-named executable:
check_run('noop=1.0-default')

# Check success using a single, non-default named executable:
check_run('noop=1.0-nondef')

# Try running when more than one exec, error must happen
check_run('noop=1.0-multi', match=".*No executable specified but the release builds more than one executable.*")


print('SUCCESS')
