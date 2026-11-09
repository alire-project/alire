"""
Test that `alr index --check` warns about VCS mirrors on untrusted hosts, as
they are pinned by commit (SHA-1), without failing (mirrors are fallbacks, and
whether to use an untrusted one is decided when it is used). Archive mirrors,
pinned by stronger hashes, may be on any host.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match, assert_not_substring

p = run_alr("index", "--check", quiet=False)  # Succeeds

assert_match(".*Warning: Release untrusted_mirror=1.0.0 has mirror URL not in"
             " known hosts: https://example.com/someone/crate.git", p.out)
assert_not_substring("trusted_mirror=1.0.0 has", p.out.replace(
    "untrusted_mirror=1.0.0 has", ""))
assert_not_substring("archive_mirror", p.out)
assert_not_substring("Issues were found in index contents", p.out)

print("SUCCESS")
