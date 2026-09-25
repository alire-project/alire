"""Exercise additive crate features, including weak feature forwarding."""

import os

from drivers.alr import alr_lockfile, init_local_crate, run_alr
from drivers.asserts import assert_in_file, assert_not_substring, assert_substring
from drivers.helpers import content_of


def append_manifest(crate, text):
    with open(os.path.join(crate, "alire.toml"), "a") as manifest:
        manifest.write(text)


def lock():
    return content_of(alr_lockfile())


# A dependency with one observable feature.
init_local_crate("feature_leaf", binary=False, enter=False, update=False)
append_manifest("feature_leaf", """

[features]
default = []
special = []
""")

# A crate whose weak forwarding and activation are deliberately separate.
init_local_crate("feature_owner", binary=False, enter=False, update=False)
append_manifest("feature_owner", """

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[features]
default = []
weak = ["feature_leaf?/special"]
activate = ["dep:feature_leaf"]
strong = ["feature_leaf/special"]
""")

# A dependency whose default feature has an observable optional dependency.
init_local_crate("defaulted_owner", binary=False, enter=False, update=False)
append_manifest("defaulted_owner", """

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[features]
default = ["dep:feature_leaf"]
""")

# Two independent parents request features on the same owner release.
init_local_crate("weak_parent", binary=False, enter=False, update=False)
append_manifest("weak_parent", """

[[depends-on]]
feature_owner = { version = "*", features = ["weak"], default-features = false }
""")

init_local_crate("activate_parent", binary=False, enter=False, update=False)
append_manifest("activate_parent", """

[[depends-on]]
feature_owner = { version = "*", features = ["activate"], default-features = false }
""")

# Weak forwarding alone must not activate the optional dependency. The root's
# otherwise-unused declaration lets it own the leaf path pin while leaving the
# dependency inactive.
init_local_crate("weak_root", enter=True, update=False)
append_manifest(".", """

[[depends-on]]
weak_parent = "*"

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[[pins]]
weak_parent = { path = "../weak_parent" }

[[pins]]
feature_owner = { path = "../feature_owner" }

[[pins]]
feature_leaf = { path = "../feature_leaf" }
""")
run_alr("update")
assert_substring('crate = "feature_owner"', lock())
assert_not_substring('crate = "feature_leaf"', lock())
os.chdir("..")

# Pin constraints are version/location constraints only. They must not turn a
# dependency's explicitly disabled default features back on.
init_local_crate("no_dependency_defaults", enter=True, update=False)
append_manifest(".", """

[[depends-on]]
defaulted_owner = { version = "*", default-features = false }

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[[pins]]
defaulted_owner = { path = "../defaulted_owner" }

[[pins]]
feature_leaf = { path = "../feature_leaf" }
""")
run_alr("update")
assert_substring('crate = "defaulted_owner"', lock())
assert_not_substring('crate = "feature_leaf"', lock())
os.chdir("..")

# Requests from separate edges are globally unified. Activation on the second
# edge makes the first edge's weak forwarding take effect.
init_local_crate("unified_root", enter=True, update=False)
append_manifest(".", """

[[depends-on]]
weak_parent = "*"

[[depends-on]]
activate_parent = "*"

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[[pins]]
weak_parent = { path = "../weak_parent" }

[[pins]]
activate_parent = { path = "../activate_parent" }

[[pins]]
feature_owner = { path = "../feature_owner" }

[[pins]]
feature_leaf = { path = "../feature_leaf" }
""")
run_alr("update")
assert_substring('crate = "feature_leaf"', lock())
assert_substring('"special",', lock())
os.chdir("..")

# Root defaults, explicit selection, weak forwarding, generated feature
# constants, and lockfile selection all use the same resolved state.
init_local_crate("root_features", enter=True, update=False)
append_manifest(".", """

[[depends-on]]
feature_leaf = { version = "*", optional = true }

[[pins]]
feature_leaf = { path = "../feature_leaf" }

[features]
default = ["activate"]
activate = ["dep:feature_leaf"]
weak = ["feature_leaf?/special"]
""")

run_alr("update")
run_alr("build", "--stop-after=generation")
assert_substring('crate = "feature_leaf"', lock())
assert_in_file("config/root_features_config.ads",
               "Feature_Default : constant Boolean := True;")
assert_in_file("config/root_features_config.ads",
               "Feature_Activate : constant Boolean := True;")

run_alr("build", "--no-default-features", "--features=weak",
        "--stop-after=generation")
assert_not_substring('crate = "feature_leaf"', lock())
assert_substring("default_features = false", lock())
assert_in_file("config/root_features_config.ads",
               "Feature_Weak : constant Boolean := True;")

run_alr("build", "--no-default-features", "--features=weak,activate",
        "--stop-after=generation")
assert_substring('crate = "feature_leaf"', lock())
assert_substring('"special",', lock())

# A build without feature switches adopts the stored root selection instead of
# resetting it to defaults.
run_alr("build", "--stop-after=generation")
assert_substring("default_features = false", lock())
assert_substring('"activate",', lock())
assert_substring('"weak",', lock())
assert_substring('crate = "feature_leaf"', lock())
assert_substring('"special",', lock())
os.chdir("..")

# `alr with` persists edge-local feature requests in the manifest.
init_local_crate("with_features", enter=True, update=False)
run_alr("with", "feature_leaf", "--use=../feature_leaf",
        "--features=special", "--no-default-features", force=True)
manifest = content_of("alire.toml")
assert_substring("default-features = false", manifest)
assert_substring('features = ["special"]', manifest)

print("SUCCESS")
