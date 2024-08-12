"""
Check that publish rejects 'ssh://' and 'git+ssh://' origins as private
"""


from drivers.alr import run_alr
from drivers.asserts import assert_match


# We expect attempts to publish from these origins to fail
urls = [
    "git+ssh://host.invalid/path/to/repo.git",
    "ssh://host.invalid/path/to/repo.git",
]
for url in urls:
    commit = "0"*40
    p = run_alr("publish", url, commit, complain_on_error=False)
    assert_match(r".*The origin cannot use a private remote:.*", p.out)


print('SUCCESS')
