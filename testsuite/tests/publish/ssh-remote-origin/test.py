"""
Check "alr publish <url>" only allows private origins with --for-private-index
"""


from drivers.alr import run_alr
from drivers.asserts import assert_match


urls = [
    "git+ssh://host.invalid/path/to/repo.git",
    "ssh://host.invalid/path/to/repo.git",
    "git@host.invalid:/path/to/repo.git",
]
commit = "0" * 40

# We expect attempts to publish from these origins to fail, because they are
# obviously private.
for url in urls:
    p = run_alr("publish", url, commit, complain_on_error=False)
    assert_match(r".*The origin cannot use a private remote:.*", p.out)
    p = run_alr(
        "publish", "--skip-submit", url, commit, complain_on_error=False
    )
    assert_match(r".*The origin cannot use a private remote:.*", p.out)

# Publishing will still fail with "--for-private-index", but it should be due to
# the untrusted host, not because the URLs appear private.
for url in urls:
    p = run_alr(
        "publish", "--for-private-index", url, commit,complain_on_error=False
    )
    assert_match(r".*Origin is hosted on unknown site: host\.invalid.*", p.out)


print("SUCCESS")
