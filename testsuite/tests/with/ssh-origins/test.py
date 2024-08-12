"""
Check that 'ssh://' and 'git+ssh://' crate origins are not considered invalid
"""


from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match


# We expect attempts to get crates from these origins to fail, but it should be
# due to the invalid host, not because of the scheme
init_local_crate(update=False)
for lib_name in ["libfoo", "libbar", "libbaz"]:
    p = run_alr("with", lib_name, complain_on_error=False)
    assert_match(
        r".*Could not resolve hostname host.invalid.*",
        p.out
    )


print("SUCCESS")
