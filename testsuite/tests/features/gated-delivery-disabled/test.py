"""Additive package features fail closed while their gate is disabled."""

import os

from drivers.alr import alr_lockfile, init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import content_of


gate = "ALIRE_GATE_FEATURES"
os.environ.pop(gate, None)

# Index 1.5 remains readable for ordinary commands. Feature-bearing releases
# are retained for inspection but excluded from dependency solving.
p = run_alr("search", "--crates", quiet=False)
assert_substring("trial_features", p.out)
assert_substring("feature-bearing releases will be ignored", p.out)
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)
assert p.out.count("feature-bearing releases will be ignored") == 1
assert "older than the preferred version" not in p.out

# Direct solver lookups used by toolchain selection must not bypass the gate.
p = run_alr("toolchain", "--select", "trial_features",
            complain_on_error=False)
assert p.status != 0, "direct solver lookup should exclude gated releases"
assert_substring("Release within requested versions not found", p.out)

# The interactive assistant's candidate query is gated independently too.
p = run_alr("toolchain", "--select", quiet=False)
assert "gnat_native=1.0.0" not in p.out

init_local_crate("gated_features", enter=True, update=False)

# Both CLI entry points reject explicit selections before changing a crate.
p = run_alr("with", "trial_features", "--features=extra",
            complain_on_error=False)
assert p.status != 0, "dependency feature selection should be gated off"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)

p = run_alr("build", "--features=extra", complain_on_error=False)
assert p.status != 0, "feature selection should be gated off by default"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)

# Inline dependency feature syntax is gated in local manifests even when all
# settings happen to use their defaults.
with open("alire.toml") as manifest:
    plain_manifest = manifest.read()
with open("alire.toml", "a") as manifest:
    manifest.write("""

[[depends-on]]
trial_features = { version = "*" }
""")
p = run_alr("update", complain_on_error=False)
assert p.status != 0, "inline dependency syntax should be gated off"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)

# Even an empty feature table is gated syntax.
with open("alire.toml", "w") as manifest:
    manifest.write(plain_manifest)
    manifest.write("""

[features]
""")
p = run_alr("build", complain_on_error=False)
assert p.status != 0, "feature syntax should be gated off by default"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)

# A legacy dependency on a feature-bearing release remains unsolved without
# the gate; merely reading the 1.5 index does not activate feature behavior.
with open("alire.toml", "w") as manifest:
    manifest.write(plain_manifest)
    manifest.write("""

[[depends-on]]
trial_features = "*"
""")
run_alr("update")
assert "[solution.state.release]" not in content_of(alr_lockfile())

print("SUCCESS")
