"""
check that a linked dependency causes no trouble in shared builds
"""

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers import builds
# from drivers.asserts import assert_eq, assert_match

builds.enable_shared()

init_local_crate()
init_local_crate("dep", enter=False)
alr_with("dep", path="dep")

run_alr("build")  # Should succeed

print('SUCCESS')
