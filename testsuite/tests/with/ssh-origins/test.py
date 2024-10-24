"""
Check that 'ssh://' and 'git+ssh://' crate origins are not considered invalid
"""


from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match


# We expect attempts to get crates from these origins to fail, but it should be
# because the git clone command fails, not because Alire rejects the origin URL.
init_local_crate(update=False)
for lib_name in ["libfoo", "libbar", "libbaz"]:
    p = run_alr("with", lib_name, complain_on_error=False)
    assert_match(
        (
            r'.*Command \["git", "clone", "--recursive", "-q", '
            r'"ssh://host\.invalid/path/to/repo.git", ".*"\] '
            r'exited with code 128'
        ),
        p.out
    )


print("SUCCESS")
