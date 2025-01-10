"""
Check detection of untrusted hosts when running `alr index --check`
"""


import os

from drivers.alr import run_alr, alr_settings_set
from drivers.asserts import assert_eq, assert_match
from drivers.helpers import replace_in_file


# `alr index --check` should fail due to untrusted host.
p = run_alr("index", "--check", quiet=False, complain_on_error=False)
assert_match(
    (
        r"Warning: Release crate=1\.0\.0 has URL not in known hosts: "
        r"https://some\.host/path/to/repo"
        r".*ERROR: Issues were found in index contents"
    ),
    p.out
)

# Configure the untrusted host as trusted.
alr_settings_set("origins.git.trusted_sites", "github.com some.host  gitlab.com")

# `alr index --check` should now succeed.
p = run_alr("index", "--check", quiet=False)
assert_eq("Success: No issues found in index contents.\n", p.out)

# Change the index manifest to use a new untrusted host, and check that
# `alr index --check` fails again.
manifest_path = os.path.join(
    "my_index", "index", "cr", "crate", "crate-1.0.0.toml"
)
replace_in_file(
    manifest_path,
    "git+https://some.host/path/to/repo",
    "git+https://untrusted.host/path/to/repo"
)
p = run_alr("index", "--check", quiet=False, complain_on_error=False)
assert_match(
    (
        r"Warning: Release crate=1\.0\.0 has URL not in known hosts: "
        r"https://untrusted\.host/path/to/repo"
        r".*ERROR: Issues were found in index contents"
    ),
    p.out
)

# Verify that it still fails when the host has a trailing dot (note the double
# space in the setting value, which was causing the empty string to be
# considered a trusted host, and hence `untrusted.host.` to be considered a
# trusted subdomain thereof).
replace_in_file(
    manifest_path,
    "git+https://untrusted.host/path/to/repo",
    "git+https://untrusted.host./path/to/repo"
)
p = run_alr("index", "--check", quiet=False, complain_on_error=False)
assert_match(
    (
        r"Warning: Release crate=1\.0\.0 has URL not in known hosts: "
        r"https://untrusted\.host\./path/to/repo"
        r".*ERROR: Issues were found in index contents"
    ),
    p.out
)

# Set 'origins.git.trusted_sites' to ' ' and verify that all hosts are now
# permitted.
alr_settings_set("origins.git.trusted_sites", " ")
p = run_alr("index", "--check", quiet=False)
assert_eq("Success: No issues found in index contents.\n", p.out)


print("SUCCESS")
