"""
Test that binary files containing softlinks can be installed properly
"""

from drivers.alr import run_alr

# This command should succeed normally
run_alr("install", "--prefix=install", "crate")


print('SUCCESS')
