"""
Check that publish gives appropriate errors for URIs with a "file:" scheme
"""


from drivers.alr import run_alr
from drivers.asserts import assert_match


# With commit argument
commit = "0"*40
p = run_alr("publish", "file:/path/to/repo/", commit, complain_on_error=False)
assert_match(r".*unknown VCS URL: file:/path/to/repo/.*", p.out)

# Without commit argument (path to file)
p = run_alr("publish", "file:/path/to/repo", complain_on_error=False)
assert_match(
    r".*Unable to determine archive format from file extension.*",
    p.out
)

# Without commit argument (path to directory)
p = run_alr("publish", "file:/path/to/repo/", complain_on_error=False)
assert_match(r".*Unable to determine archive name: please specify one.*", p.out)


print('SUCCESS')
