"""
Test the policy and allowlist for running version commands of externals
"""

import os

from drivers.alr import (alr_settings_dir, alr_settings_set, run_alr,
                         use_index_with_manifests)
from drivers.asserts import assert_match, assert_substring, assert_not_substring


def external(name, command):
    return f"""
description = "An external detected from its version output"
name = "{name}"
licenses = "GPL-3.0-only"
maintainers = ["example@example.com"]
maintainers-logins = ["mylogin"]

[[external]]
kind = "version-output"
version-regexp = ".*?([\\\\d\\\\.]+).*"
version-command = {command}
"""


use_index_with_manifests({
    "make-external.toml": external("make", '["make", "--version"]'),
    "echoer-external.toml": external("echoer", '["echo", "1.2.3"]'),
})


def detect(crate):
    return run_alr("show", crate, "--external-detect", quiet=False).out


def assert_detected(crate, wanted=True):
    output = detect(crate)
    if wanted:
        assert_match(f".*Origin: external path .*", output)
    else:
        assert_match(f".*Not found: {crate}", output)
    return output


# The testsuite runs every command by default; restore alr's default policy,
# under which only commands in the default allowlist are run.
alr_settings_set("externals.version_output.policy", "allowlist")

assert_detected("make")
output = assert_detected("echoer", wanted=False)
assert_substring("Not running version command [echo 1.2.3] of external "
                 "echoer as it is not in externals.version_output.allowlist",
                 output)

# A wildcard matches the argument
alr_settings_set("externals.version_output.allowlist",
                 "make --version; echo *")
assert_detected("make")
output = assert_detected("echoer")
assert_not_substring("Not running", output)

# Replacing the allowlist drops the default entries
alr_settings_set("externals.version_output.allowlist", "echo *")
assert_detected("make", wanted=False)
assert_detected("echoer")

# None runs nothing, silently
alr_settings_set("externals.version_output.policy", "none")
for crate in ["make", "echoer"]:
    assert_not_substring("Not running", assert_detected(crate, wanted=False))

# All runs anything, regardless of the allowlist
alr_settings_set("externals.version_output.allowlist", "none")
alr_settings_set("externals.version_output.policy", "all")
assert_detected("make")
assert_detected("echoer")

# Invalid policies are rejected
p = run_alr("settings", "--global", "--set",
            "externals.version_output.policy", "some", complain_on_error=False)
assert p.status != 0, "unexpected success"
assert_substring("invalid value: 'some'", p.out)

# An invalid policy edited into the settings file is reported and ignored, so
# the default policy applies (the allowlist matches nothing at this point)
settings_file = os.path.join(alr_settings_dir(), "settings.toml")
with open(settings_file) as f:
    contents = f.read()
assert_substring('policy = "all"', contents)
with open(settings_file, "w") as f:
    f.write(contents.replace('policy = "all"', 'policy = "some"'))

output = assert_detected("make", wanted=False)
assert_substring("Invalid value 'some' for builtin configuration "
                 "'externals.version_output.policy'", output)
assert_substring("Not running version command [make --version]", output)

print('SUCCESS')
