"""
Validate just `alr` is printing something that makes sense and not throwing
some initialization exception during elaboration as sometimes has happened
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Check the first line looks like a version and then comes the USAGE
assert_match("^alr \S*?\n\nUSAGE.*",
             run_alr(complain_on_error=False).out)

print('SUCCESS')
