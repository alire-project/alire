"""Dependency feature syntax requires an index new enough to parse it."""

from pathlib import Path

from drivers.alr import run_alr
from drivers.asserts import assert_substring


p = run_alr("search", "--crates", complain_on_error=False)
assert p.status != 0, "the 1.4 index should have been rejected"
assert_substring("crate features require index version 1.5.0 or newer", p.out)

# An explicitly present but empty feature table is still 1.5 syntax.
manifest = Path("my_index/index/fe/feature_gate/feature_gate-1.0.0.toml")
contents = manifest.read_text()
dependency = """[[depends-on]]
plain_dependency = { version = "*", default-features = false }
"""
manifest.write_text(contents.replace(dependency, "[features]\n"))

p = run_alr("search", "--crates", complain_on_error=False)
assert p.status != 0, "an empty feature table should require index 1.5"
assert_substring("crate features require index version 1.5.0 or newer", p.out)

print("SUCCESS")
