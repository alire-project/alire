"""
Test the loading of case expressions without resolving them.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match
from drivers.helpers import host_architecture

import re
import platform

p = run_alr('show', 'hello')

# If the index loads without complaints we are already in promising territory.
# Check a few substrings for more certainty:

# Available
assert_match(".*Available when: .case OS is Linux => True, "
             "Windows => \(case Word_Size is Bits_64 => True, others => False\)"
             ", others => False.*",
             p.out, flags=re.S)

# Properties

assert_match(".*case Word_Size is .* when Bits_32 => Executable: hello32.*",
             p.out, flags=re.S)

assert_match(".*case OS is .* when Linux => GPR External: OS := linux.*",
             p.out, flags=re.S)

# Dependencies
assert_match(".*Dependencies .direct.:.*case OS is.*when Linux => libhello\^1.*",
             p.out, flags=re.S)

# Check that evaluation for the current platform does work
p = run_alr('show', 'hello', '--system')

# And that, once resolved, the expected property is there:
if platform.system() == 'Windows':
    assert_match(".*GPR External: OS := windows.*",
                 p.out, flags=re.S)
elif platform.system() == 'Darwin':
    assert_match(".*GPR External: OS := macos.*",
                 p.out, flags=re.S)
else:
    assert_match(".*GPR External: OS := linux.*",
                 p.out, flags=re.S)

assert_match(".*GPR External: HOST_ARCH := " + host_architecture() + ".*",
             p.out, flags=re.S)

# Check that a case given as "x|y" is properly loaded and shown
p = run_alr("show", "hello=0.9")
assert_match(
    '.*'
    'Properties:\n'
    '   Description: "Hello, world!" demonstration project\n'
    '   case OS is\n'
    '      when Linux => Executable: hello\n'
    '      when Windows => Executable: hello\n'
    '.*',
    p.out)

print('SUCCESS')
