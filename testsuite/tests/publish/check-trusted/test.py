"""
Tests that the "trusted repos" list is applied
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Try with obvious bad site and slight variations before/after
for domain in ["badsite.com", "ggithub.com", "github.comm"]:
    p = run_alr("publish", f"http://{domain}/repo.git",
                "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
                "--skip-submit",
                complain_on_error=False)
    assert_match(f".*Origin is hosted on unknown site: {domain}.*", p.out)

# Try that having credentials doesn't interfere with the previous check and
# that the domain was recognized properly
for domain in ["badsite.com", "ggithub.com", "github.comm"]:
    for creds in ["user", "user:passwd"]:
        p = run_alr("publish", f"http://{creds}@{domain}/repo.git",
                    "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
                    "--skip-submit",
                    complain_on_error=False)
        assert_match(f".*Origin is hosted on unknown site: {domain}.*", p.out)

print('SUCCESS')
