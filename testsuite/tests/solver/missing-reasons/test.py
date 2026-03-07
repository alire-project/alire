"""
Test missing reasons in solutions
"""

import os

from drivers.alr import init_local_crate, alr_with, alr_pin
from drivers.asserts import match_solution

# Conflict between hello versions, nonexistent libhello, and nonexistent crate

# Root crate
init_local_crate()

# Dependency to force conflict on hello
init_local_crate("dep")
alr_with("hello=1.0.0")
os.chdir("..")

alr_with("hello=1.0.1")    # Conflict with dep -> hello=1.0.0
alr_with("superhello=777") # Version not in index
alr_with("unobtanium")     # Unindexed crate

alr_pin("dep", path="dep")  # Establish the conflict with this local dep

match_solution(
    """Dependencies (missing):
   hello(=1.0.1) & (=1.0.0) (direct,missed:conflict)
   superhello=777 (direct,missed:unavailable)
   unobtanium* (direct,missed:unknown)""",
    escape=True)

print('SUCCESS')
