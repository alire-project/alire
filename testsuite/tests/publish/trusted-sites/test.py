"""
Check `alr publish --trusted-sites`
"""


from drivers.alr import run_alr, alr_settings_set
from drivers.asserts import assert_eq, assert_substring


# Configure a non-default trusted sites list
alr_settings_set("origins.git.trusted_sites", "some.host other.host third.host")

# Verify that `alr publish --trusted-sites` prints the hardcoded list of hosts
# trusted by the community index
p = run_alr("publish", "--trusted-sites")
assert_substring("\ngithub.com\n", p.out)

# Verify that `alr publish --for-private-index --trusted-sites` prints the
# configured list
p = run_alr("publish", "--for-private-index", "--trusted-sites")
assert_eq("some.host\nother.host\nthird.host\n", p.out)

# Set `origins.git.trusted_sites` to '...' (which trusts all hosts)
alr_settings_set("origins.git.trusted_sites", "...")

# Verify that the output of `alr publish --trusted-sites` is unchanged
p = run_alr("publish", "--trusted-sites")
assert_substring("\ngithub.com\n", p.out)

# Verify that `alr publish --for-private-index --trusted-sites` prints a
# suitable message
p = run_alr("publish", "--for-private-index", "--trusted-sites")
assert_eq("All sites are currently trusted for private indexes.\n", p.out)


print('SUCCESS')
