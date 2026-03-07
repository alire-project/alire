"""
Verify that unsatisfiable crates do not result in a trivial solution being
returned, where all/part of the dependencies are skipped. This happened when we
only stored potentially complete solutions. If a crate is unsatisfiable, no
solution is potentially complete and the first partial solution to date was
returned.

We test here with an external and a regular crate (the same problem arose)

TL;DR: If a dependency is unsatisfiable, we should still solve the rest of the
dependencies.

Example of wrong solution to avoid (note the missed:skipped for a perfectly
solvable dependency):

Dependencies (direct):
   hello^1.0.2
   missing_external*
Dependencies (solution):
   hello=1.0.2 (origin: git)
Dependencies (missing):
   libhello^1.0 (indirect,missed:skipped)
   missing_external* (direct,missed:unknown)
Dependencies (graph):
   hello=1.0.2   --> libhello^1.0
   xxx=0.1.0-dev --> hello=1.0.2 (^1.0.2)
   xxx=0.1.0-dev --> missing_external*

"""

from drivers.alr import alr_with, init_local_crate, run_alr
from drivers.asserts import match_solution

# Test with both an undetectable external and a regular missing crate

for missing in ["missing_external", "unobtainium"]:
    init_local_crate()
    alr_with("hello")
    alr_with(missing)

    # Note in the following test that for externals we hint that it is an
    # external. Perhaps 'hinted' should be changed into 'missing:system' to be
    # more explicit about what is happening.

    match_solution(f"""\
Dependencies (solution):
   hello=1.0.1 (origin: filesystem)
   libhello=1.0.0 (origin: filesystem)
Dependencies (missing):
   {missing}* (direct,{'hinted' if missing == 'missing_external' else 'missed:unknown'})
""", escape=True)

print("SUCCESS")
