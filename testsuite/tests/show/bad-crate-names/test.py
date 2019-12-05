# -*- coding: utf-8 -*-
"""
Test lowercaseness of crate names, and other badnesses.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_match

import re

assert_match(".*Identifiers must be lowercase ASCII alphanumerical.*",
             run_alr('show', 'HELLO', complain_on_error=False).out, flags=re.S)

assert_match(".*Identifiers must be lowercase ASCII alphanumerical.*",
             run_alr('show', 'ññññ', complain_on_error=False).out, flags=re.S)

assert_match(".*Identifier too short.*",
             run_alr('show', 'xx', complain_on_error=False).out, flags=re.S)

assert_match(".*Identifier too long.*",
             run_alr('show', 'x'*65, complain_on_error=False).out, flags=re.S)

assert_match(".*Identifiers must not begin with an underscore.*",
             run_alr('show', '_xxx', complain_on_error=False).out, flags=re.S)

print('SUCCESS')
