"""The delivery gate enables features and protects their persisted state."""

import os
from shutil import rmtree

from drivers.alr import alr_lockfile, alr_settings_dir, init_local_crate, run_alr
from drivers.asserts import assert_substring
from drivers.helpers import content_of


gate = "ALIRE_GATE_FEATURES"
os.environ[gate] = ""  # Presence enables the gate; the value is ignored.

# Toolchain selection may consume a gated release while enabled, but a later
# ungated process must not silently redeploy it from raw index queries.
run_alr("toolchain", "--select", "gnat_native=1")
init_local_crate("gated_tool_redeploy", enter=True, update=False)
rmtree(os.path.join(alr_settings_dir(), "cache", "toolchains"))
os.environ.pop(gate)
p = run_alr("printenv", complain_on_error=False)
assert p.status != 0, "gated configured tool should not be redeployed"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)
os.chdir("..")

os.environ[gate] = ""
init_local_crate("gated_features", enter=True, update=False)
with open("alire.toml") as manifest:
    plain_manifest = manifest.read()
with open("alire.toml", "a") as manifest:
    manifest.write("""

[features]
default = []
extra = []
""")
run_alr("build", "--features=extra", "--stop-after=generation")

# Reopening gated root feature state without the gate fails without treating
# the lockfile as corrupt or replacing it.
with open("alire.toml", "w") as manifest:
    manifest.write(plain_manifest)
locked = content_of(alr_lockfile())
os.environ.pop(gate)
p = run_alr("build", complain_on_error=False)
assert p.status != 0, "stored root feature selection should remain gated"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)
assert locked == content_of(alr_lockfile()), "gated lockfile was replaced"

# The same protection applies to a solved feature-bearing index release.
os.chdir("..")
os.environ[gate] = "enabled"
init_local_crate("gated_dependency_lock", enter=True, update=False)
with open("alire.toml", "a") as manifest:
    manifest.write("""

[[depends-on]]
trial_features = "*"
""")
run_alr("update")
locked = content_of(alr_lockfile())
os.environ.pop(gate)
p = run_alr("build", "--stop-after=generation", complain_on_error=False)
assert p.status != 0, "stored feature-bearing release should remain gated"
assert_substring("set ALIRE_GATE_FEATURES to enable", p.out)
assert locked == content_of(alr_lockfile()), "gated lockfile was replaced"

print("SUCCESS")
