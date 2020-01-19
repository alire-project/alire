"""
Test two outcomes of requesting an unavailable native package:
    1) In unknown distros, say that there is no support
    2) In known distros, say the package is unavailable
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

# Ascertain first if we are in a known or unknown platform:
p = run_alr('version')
unknown = re.match('.*platform properties:.*DISTRO_UNKNOWN.*',
                   p.out, flags=re.S)

# Run get on a native package and see what happens depending on platform
p = run_alr('get', '--non-interactive', '--only', 'make',
            complain_on_error=False, quiet=False)
if unknown:
    assert_match(".*Unknown distribution: cannot use native package for.*",
                 p.out, flags=re.S)
else:
    assert_match(".*No native package for the requested crate was detected.*",
                 p.out, flags=re.S)

print('SUCCESS')
