"""
A release already indexed is not replaced by a release with the same version
found later (e.g. in a lower-priority index). Neither must it be replaced
through the crates it provides, or a dependency on a provided crate would
silently resolve to the release that was not indexed.
"""

from drivers.alr import init_local_crate, run_alr
from drivers.helpers import content_of

# Both indexes have ccc=1.0.0 providing xxx; the high-priority one wins
init_local_crate()
run_alr("with", "xxx", force=True)

lockfile = content_of("alire/alire.lock")
assert 'description = "ccc from high index"' in lockfile, \
    "Dependency on xxx not solved with the indexed release:\n" + lockfile

print("SUCCESS")
