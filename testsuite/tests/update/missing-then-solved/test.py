"""
A dependency that was missing and becomes available is shown as solved in the
changes to the solution.
"""

from drivers.alr import fixtures_path, init_local_crate, run_alr
from drivers.asserts import assert_substring

init_local_crate()

# Not in the configured index yet, so it is missing in the solution
run_alr("with", "superhello", force=True)

run_alr("index", "--add", fixtures_path("solver_index"), "--name", "solver")
p = run_alr("update", quiet=False)
assert_substring("superhello 1.0.0 (solved)", p.out)

print("SUCCESS")
