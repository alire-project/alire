"""
Check that index manifests can load releases with top-level mirror tables.
"""

from drivers.alr import run_alr


# Showing the crate should succeed, as there is a mirror defined
p = run_alr("show", "crate")
assert "crate=1.0.0" in p.out, f"Unexpected output: {p.out}"


print("SUCCESS")
