"""
Test the versions found in the JSON output of `brew info` for system externals,
for packages that are installed, available but not installed, or not found.
This is to ensure consistent JSON content no matter which lib is used for JSON.
"""

from drivers.alr import alr_settings_set, run_alr, use_index_with_manifests
from drivers.asserts import assert_match, assert_not_substring
from drivers.helpers import MockCommand

MANIFEST = """
name = "{name}"
description = "External from Homebrew"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[[external]]
kind = "system"
origin = ["{name}"]
"""

BREW = """\
import json, sys

name = sys.argv[-1]
if name == "libmissing":
    print("Error: No available formula with the name", name)
    sys.exit(1)

print(json.dumps(
    [{"name": name,
      "desc": "Some \\"text\\" with\\nescapes and \\u00f1",
      "aliases": [],
      "revision": 0,
      "license": None,
      "versions": {"stable": "1.2.4", "head": None, "bottle": True},
      "installed": [{"version": "1.2.3",
                     "used_options": [],
                     "time": 1700000000,
                     "runtime_dependencies": [{"full_name": "x"}]}]
      if name == "libinstalled" else []}],
    indent=2))
"""

use_index_with_manifests({
    f"{name}-external.toml": MANIFEST.format(name=name)
    for name in ["libinstalled", "libavailable", "libmissing"]})

alr_settings_set("distribution.disable_detection", "false")
alr_settings_set("distribution.override", "homebrew")

with MockCommand("brew", BREW, "mocks"):
    for name, version in [("libinstalled", "1.2.3"),
                          ("libavailable", "1.2.4")]:
        assert_match(f".*{name}={version}.*",
                     run_alr("show", name, "--external-detect").out)

    assert_not_substring("libmissing=",
                         run_alr("show", "libmissing",
                                 "--external-detect").out)

print("SUCCESS")
