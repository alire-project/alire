"""
Check that ambiguous ssh origins yield a suggestion
"""


from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match


# This origin has an "ssh://" scheme, but neither a "git+" prefix nor a ".git"
# suffix, so should fail with a prefix suggestion
init_local_crate(update=False)
p = run_alr("with", "libqux", complain_on_error=False)
assert_match(
    (
        r".*Pure 'ssh://' URLs are not valid crate origins. You may want "
        r"git\+ssh://host.invalid/path/to/repo_without_suffix instead.*"
    ),
    p.out
)


print("SUCCESS")
