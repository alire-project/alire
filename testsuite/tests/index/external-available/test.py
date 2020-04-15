"""
Test use of the available field in externals
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import platform

# 1st test: showing available information on all platforms

p = run_alr('show', 'crate', '--external')

assert_match(".*Executable make --version .*"
             "(case Toolchain is SYSTEM => False, USER => False).*",
             p.out, flags=re.S)

# 2nd test: showing available information on current platform

p = run_alr('show', 'crate', '--external', '--system')

assert_match(".*Executable make --version .* False.*",
             p.out, flags=re.S)

# 3rd test: crate is not detected because it is unavailable. It would be
# detectable otherwise (make is installed in all test images)

p = run_alr('show', 'crate', '--external-detect', quiet=False)

assert_match("Looking for external crate: crate\n"
             "Not found: crate with Newest version.*",
             p.out, flags=re.S)


print('SUCCESS')
