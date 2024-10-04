"""
Check that executable attributes are synchronized correctly. We have a
dependency which launches a shell script in its pre-build step; this fails
unless the script is executable, which it should be after syncing.
"""

from drivers.alr import run_alr, init_local_crate, alr_with
from drivers.asserts import assert_eq

# Init a crate that will have the test crate as a dependency, thus causing its
# syncing.

init_local_crate()
alr_with("crate", manual=False, update=False)   # Delay syncing to capture output
p = run_alr("build", "--stop-after=pre-build")  # Gain some testing time by not building

assert_eq(p.out, "SCRIPT RUN\n")

print("SUCCESS")
