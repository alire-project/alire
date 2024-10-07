"""
Verify that we can link-pin a crate that has "provides" in a variety of ways.
"""

import os
from drivers.alr import run_alr, init_local_crate, alr_manifest
from drivers.asserts import assert_eq, assert_match, match_solution

# Create the target crate
init_local_crate(name="mylib")
# Update its manifest
with open(alr_manifest(), "a") as f:
    f.write('provides=["coollib=1.0.0"]')
os.chdir("..")

tests = [
    ["with", "--use=../mylib"],
    ["with", "coollib", "--use=../mylib"],
]

matches = [
    "mylib=0.1.0-dev (pinned) (origin: ../mylib)",
    "coollib=0.1.0-dev (mylib) (pinned) (origin: ../mylib)"
]

for target, test, id in zip(matches, tests, range(len(tests))):
    # Create a new dependent crate
    init_local_crate(f"myapp{id}")
    run_alr(*test)
    match_solution(target, escape=True)
    os.chdir("..")

# For the final test we need an extra step to force-add the dependency before
# pinning.
init_local_crate("myapp_final")
run_alr("--force", "with", "coollib")  # Force because not in index
run_alr("pin", "coollib", "--use=../mylib")
match_solution(matches[-1], escape=True)

print("SUCCESS")
