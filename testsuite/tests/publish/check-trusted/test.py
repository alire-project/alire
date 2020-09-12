"""
Tests that the "trusted repos" list is applied
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match

# Try with obvious bad site and slight variations before/after
for domain in ["badsite.com", "ggithub.com", "github.comm"]:
    p = run_alr("publish", f"http://{domain}/repo.git",
                "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
                complain_on_error=False)
    assert_match(".*Origin is hosted on unknown site.*", p.out)

print('SUCCESS')
