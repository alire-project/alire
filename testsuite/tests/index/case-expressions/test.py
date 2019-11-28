"""
Test the loading of case expressions without resolving them.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re
import os

p = run_alr('show', 'hello')

# If the index loads without complaints we are already in promising territory.
# Check a few substrings for more certainty:

# Available
assert_match(".*Available when: .case OS is LINUX => True, MACOS => False, "
             "WINDOWS => .case Word_Size is BITS_32 => False, BITS_64 => True.*",
             p.out, flags=re.S)

# Properties

assert_match(".*case Word_Size is .* when Bits_32 => .Executable: hello32.*",
             p.out, flags=re.S)

assert_match(".*case OS is .* when Linux => .GPR External: OS := linux.*",
             p.out, flags=re.S)

# Dependencies
assert_match(".*Dependencies .direct.:.*case OS is.*when Linux => .libhello\^1.*",
             p.out, flags=re.S)

# Check that evaluation for the current platform does work
p = run_alr('show', 'hello', '--native')

# And that, once resolved, the expected property is there:
if os.environ.get('OS','') == 'Windows_NT':
    assert_match(".*GPR External: OS := windows.*",
                 p.out, flags=re.S)
else:
    assert_match(".*GPR External: OS := linux.*",
                 p.out, flags=re.S)

print('SUCCESS')
