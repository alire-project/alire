"""
Check that a crate built with `get --build` is built in release mode
"""

from drivers.alr import run_alr
from drivers.asserts import assert_profile
from glob import glob

run_alr("get", "--build", "hello=1.0")

assert_profile("release", "hello", glob("hello_1.0.0_*")[0])

print('SUCCESS')
