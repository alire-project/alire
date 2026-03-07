"""
Check the `alr get --build` combo on 1st run with no available compiler. We
test under a pristine environment to avoid re-occurrence of issue #1671, which
only happens when no compiler has been selected yet, and none is available in
the environment.
"""

import subprocess
from drivers.alr import run_alr, set_default_user_settings

# First undo any testsuite compiler and index setup
set_default_user_settings()

# Remove gnat from path so no compiler is available
subprocess.run(['sudo', 'mv', '/usr/bin/gnat', '/usr/bin/gnat.bak']).check_returncode()
try:
    assert subprocess.run(['gnat', '--version']).returncode == 0, "Unexpected GNAT found"
except FileNotFoundError:
    pass

# Should succeed, issue in #1671 was that no compiler was available nor
# selected automatically as should have been the case.
p = run_alr('get', '--build', 'hello')


print('SUCCESS')
