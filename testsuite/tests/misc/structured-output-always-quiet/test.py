"""Verify that structured output is not broken by extraneous output."""

import json
from drivers.alr import alr_settings_set, init_local_crate, run_alr
from drivers.asserts import assert_substring, assert_eq, assert_not_substring


# Load an old (but compatible) index with the associated warning enabled.
alr_settings_set("warning.old_index", "true")
json_search = run_alr("--format=json", "search", "--list", quiet=False)
plain_search = run_alr("search", "--list", quiet=False)

# The warning should be included in the plain output, but not in the formatted
# output, which should parse as valid JSON.
assert_substring("is older than the newest supported by alr", plain_search.out)
assert_not_substring("is older than the newest supported by alr", json_search.out)
parsed_search = json.loads(json_search.out)
assert_eq(["hello", "libhello"], [c["name"] for c in parsed_search])


# Run `alr show` on a local crate with no toolchain configured and the toolchain
# assistant enabled.
init_local_crate(name="local_crate_name")
alr_settings_set("toolchain.assistant", "true")
p = run_alr("--format=json", "show", quiet=False)

# The toolchain assistant should not output anything that breaks JSON parsing.
assert_not_substring("toolchain", p.out)
parsed_show = json.loads(p.out)
assert_eq("local_crate_name", parsed_show["name"])


print("SUCCESS")
