"""
Every crate manifest in the fixture indices must validate against the
catalog JSON Schema (schemas/catalog-schema.yaml).
"""

import glob
import os

from drivers.alr import fixtures_path
from drivers.schema import catalog_validator, manifest_errors

validator = catalog_validator()

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
