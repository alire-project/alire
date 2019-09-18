"""
Test lowercaseness of crate names.
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq

assert_eq("ERROR: A project/version string was invalid\n"
          "ERROR: alr show unsuccessful\n",
          run_alr('show', 'HELLO', complain_on_error=False).out)

print('SUCCESS')
