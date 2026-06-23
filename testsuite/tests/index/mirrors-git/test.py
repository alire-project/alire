"""
Check that index manifests can load git releases with top-level mirrors,
reusing the origin's commit for url-only mirrors.
"""

from drivers.alr import run_alr


# Showing the crate should succeed, as the mirrors are valid
p = run_alr("show", "crate")
assert "crate=1.0.0" in p.out, f"Unexpected output: {p.out}"


print("SUCCESS")
