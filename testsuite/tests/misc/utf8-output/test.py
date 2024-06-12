# -*- coding: utf-8 -*-
# Just to be sure

"""
Verify that by default, alr prints the proper UTF-8 output
"""

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq

# Check that logging output is producing the expected valid UTF-8 output

init_local_crate()
p = run_alr("dev", "--utf8")

assert_eq("ⓘ✓\n", p.out)

print('SUCCESS')
