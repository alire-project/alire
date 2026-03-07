"""
Check that a system crate dependency can be added without issue
"""

from drivers.alr import init_local_crate, run_alr

init_local_crate()
run_alr("with", "make")

print("SUCCESS")
