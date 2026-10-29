"""
Detected versions are not always valid semantic versions: Homebrew reports
package revisions (3.8.13_2), and version-output externals report whatever
the tool prints. Relaxed parsing makes them compliant (3.8.13+-2), so that a
lockfile including them can be read back, which strictly parses versions.
"""

import re

from drivers.alr import (alr_lockfile, alr_settings_set, alr_with,
                         init_local_crate, run_alr, use_index_with_manifests)
from drivers.asserts import assert_match, assert_substring
from drivers.helpers import MockCommand

MANIFEST = """
name = "{name}"
description = "External with a noncompliant version"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[[external]]
{external}
"""

use_index_with_manifests({
    "brewed-external.toml": MANIFEST.format(
        name="brewed", external='kind = "system"\norigin = ["libbrewed"]'),
    "echoed-external.toml": MANIFEST.format(
        name="echoed", external='kind = "version-output"\n'
        'version-command = ["echo", "1.2.3_4"]\n'
        'version-regexp = "(.*)"'),
})

# Homebrew, whatever the actual distribution, reporting an installed package
# with a revision
alr_settings_set("distribution.disable_detection", "false")
alr_settings_set("distribution.override", "homebrew")
BREW = """\
print('[{"versions": {"stable": "3.8.13_2"},'
      ' "installed": [{"version": "3.8.13_2"}]}]')
"""

with MockCommand("brew", BREW, "mocks"):
    for crate, version in [("brewed", "3.8.13+-2"), ("echoed", "1.2.3+-4")]:
        assert_match(f".*{crate}={re.escape(version)}.*",
                     run_alr("show", crate, "--external-detect").out)

    # A workspace with both writes them to its lockfile, and reads it back
    init_local_crate()
    alr_with("brewed")
    alr_with("echoed")
    with open(alr_lockfile()) as f:
        lockfile = f.read()
    assert_substring('version = "3.8.13+-2"', lockfile)
    assert_substring('version = "1.2.3+-4"', lockfile)

    output = run_alr("with", "--solve").out
    assert_match(".*brewed=3.8.13\\+-2.*", output)
    assert_match(".*echoed=1.2.3\\+-4.*", output)

print("SUCCESS")
