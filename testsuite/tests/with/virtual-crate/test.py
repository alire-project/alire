"""
Verify that 'withing' a virtual crate works. In particular, replicate the usual
situation of `alr with gnat^1` where gnat is a virtual crate.
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import match_solution

init_local_crate()

# Should not fail
run_alr("with", "gnat^1")

# Verify solution (as no compiler was selected, external takes precedence)
match_solution("""Dependencies (solution):
   gnat=1.0.0 (gnat_external)"""
               , escape=True)

print("SUCCESS")
