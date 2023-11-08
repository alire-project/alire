"""
Check loading of custom actions and related changes
"""

from drivers.alr import run_alr, init_local_crate, alr_manifest, add_action
from drivers.asserts import assert_eq, assert_match
from os import chdir
from shutil import rmtree

import re


def bad_action_check(type, command, name, error_re):
    # Test in new crate as the manifest is going to be broken
    init_local_crate("abc")
    add_action(type=type, command=command, name=name)
    p = run_alr("show", complain_on_error=False)
    assert p.status != 0, "Unexpected success"
    assert_match(error_re, p.out)
    chdir("..")
    rmtree("abc")


init_local_crate()

# Add a proper custom action and verify its loading
add_action(type="on-demand", name="my-action", command=["ls"])
p = run_alr("show")
assert_match(".*" + re.escape("On_Demand (my-action) run: ls (from ${CRATE_ROOT}/.)"),
             p.out)

# Verify that regular action can also have a name
add_action(type="post-fetch", name="action-2", command=["ls"])
p = run_alr("show")
assert_match(".*" + re.escape("Post_Fetch (action-2) run: ls (from ${CRATE_ROOT}/.)"),
             p.out)

# Add an on-demand action without name and see it fails
bad_action_check(type="on-demand", command=["ls"], name="",
                 error_re=".*on-demand actions require a name")

# Bad names
bad_action_check(type="on-demand", command=["ls"], name="2nd-action",
                 error_re=".*Offending name is")
bad_action_check(type="on-demand", command=["ls"], name="bad--action",
                 error_re=".*Offending name is")
bad_action_check(type="on-demand", command=["ls"], name="-action",
                 error_re=".*Offending name is")
bad_action_check(type="on-demand", command=["ls"], name="action-",
                 error_re=".*Offending name is")
bad_action_check(type="on-demand", command=["ls"], name="x",
                 error_re=".*Offending name is")

print('SUCCESS')
