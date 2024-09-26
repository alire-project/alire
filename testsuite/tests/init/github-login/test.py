"""
Check optional input of user.github_login setting and maintainers-logins field
"""

import os
import shutil

from drivers.alr import run_alr, run_alr_interactive
from drivers.asserts import assert_eq, assert_substring, assert_not_substring


USERNAME_PROMPT = (
    r"If you intend to publish this crate to the community index, you will "
    r"need a (\r\n|\r|\n)GitHub account with which to submit a pull request, "
    r"which can optionally be (\r\n|\r|\n)configured now \(leave blank to "
    r"skip\)\.(\r\n|\r|\n)Please enter your GitHub login: \(default: ''\)"
)


# `alr init` a crate without specifying a login. The resulting manifest should
# not contain a `maintainers-logins` field, and user.github_login should remain
# unset.
outputs, inputs = zip(*[
    ("Select the kind of crate you want to create",  ""),
    ("Enter a short description of the crate",       ""),
    ("Please enter your full name",                  ""),
    (USERNAME_PROMPT,                                ""),
    ("Please enter your email address",              ""),
    ("Select a software license for the crate",      ""),
    ("Enter a comma \(','\) separated list of tags", ""),
    ("Enter an optional Website URL for the crate",  ""),
])
run_alr_interactive(
    ['init', 'xxx'],
    output=outputs,
    input=inputs,
    timeout=3
)
assert_eq(
    "\n",
    run_alr("settings", "--global", "user.github_login").out
)
with open(os.path.join("xxx", "alire.toml")) as f:
    assert_not_substring('maintainers-logins', f.read())

# Clean up for next test
shutil.rmtree("xxx")

# Check inputs which aren't valid GitHub logins are rejected, then check
# configuring a valid login. The configured login should appear in the manifest
# file, and in the output of `alr settings` under `user.github_login`.
outputs, inputs = zip(*[
    ("Select the kind of crate you want to create",     ""                  ),
    ("Enter a short description of the crate",          ""                  ),
    ("Please enter your full name",                     ""                  ),
    (USERNAME_PROMPT,                                   "invalid_for_GitHub"),
    (r"Invalid answer.[\r\n]+Please enter your GitHub", "valid-user-name"   ),
    ("Please enter your email address",                 ""                  ),
    ("Select a software license for the crate",         ""                  ),
    ("Enter a comma \(','\) separated list of tags",    ""                  ),
    ("Enter an optional Website URL for the crate",     ""                  ),
])
run_alr_interactive(
    ['init', 'xxx'],
    output=outputs,
    input=inputs,
    timeout=3
)
assert_eq(
    "user.github_login=valid-user-name\n",
    run_alr("settings", "--global", "user.github_login").out
)
with open(os.path.join("xxx", "alire.toml")) as f:
    assert_substring('maintainers-logins = ["valid-user-name"]', f.read())
shutil.rmtree("xxx")

# Now that a username has been configured, check that the prompt is skipped
# and user.github_login is used instead.
outputs, inputs = zip(*[
    ("Select the kind of crate you want to create",     ""),
    ("Enter a short description of the crate",          ""),
    ("Please enter your full name",                     ""),
    ("Please enter your email address",                 ""),
    ("Select a software license for the crate",         ""),
    ("Enter a comma \(','\) separated list of tags",    ""),
    ("Enter an optional Website URL for the crate",     ""),
])
run_alr_interactive(
    ['init', 'xxx'],
    output=outputs,
    input=inputs,
    timeout=3
)
assert_eq(
    "user.github_login=valid-user-name\n",
    run_alr("settings", "--global", "user.github_login").out
)
with open(os.path.join("xxx", "alire.toml")) as f:
    assert_substring('maintainers-logins = ["valid-user-name"]', f.read())
shutil.rmtree("xxx")


print('SUCCESS')
