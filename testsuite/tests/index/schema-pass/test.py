"""
Every crate manifest in the fixture indices must validate against the
manifest schema. We don't hold invalid manifests in the fixture indices (at
least right now). Invalid cases are either in test-local indices or crafted on
the spot.
"""

import glob
import os

from drivers.alr import fixtures_path
from drivers.schemas import manifest_validator, manifest_errors

validator = manifest_validator()

# Every directory holding an index.toml is an index root; validate all of
# its crate manifests.
index_roots = {
    os.path.dirname(p)
    for p in glob.glob(fixtures_path("**", "index.toml"), recursive=True)
}
manifests = sorted(
    f
    for root in index_roots
    for f in glob.glob(os.path.join(root, "**", "*.toml"), recursive=True)
    if os.path.basename(f) != "index.toml"
)

assert manifests, "no fixture manifests found"

failures = []
for path in manifests:
    with open(path, encoding="utf-8") as f:
        errors = manifest_errors(validator, f.read())
    if errors:
        loc = "/".join(str(p) for p in errors[0].absolute_path) or "<root>"
        rel = os.path.relpath(path, fixtures_path())
        failures.append(f"{rel} @{loc}: {errors[0].message}")

# In case of failure, print all errors before asserting to fail the test
for failure in failures:
    print(f"REJECTED {failure}")

assert not failures, \
    f"{len(failures)} fixture manifest(s) failed schema validation"


print("SUCCESS")
