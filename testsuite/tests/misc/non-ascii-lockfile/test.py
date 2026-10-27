"""
Test that non-ASCII text from manifests survives being written to and read
back from the lockfile, as it did not when written through Ada.Text_IO (each
write encoded it again, until it was taken for control characters). Also, that
a lockfile mangled that way is regenerated.
"""

from drivers.alr import alr_lockfile, init_local_crate, run_alr, alr_with
from drivers.asserts import assert_match, assert_substring

init_local_crate()
alr_with("accented")

for _ in range(3):  # Each update rewrites the lockfile
    run_alr("update")

with open(alr_lockfile(), encoding="utf-8") as f:
    lockfile = f.read()
assert_substring("C\u00e9sar <cesar@example.com>", lockfile)
assert_substring("\u00e9\u20ac\u4e2d", lockfile)

run_alr("show")  # Loads the lockfile again


def mangle(times: int):
    """Encode the lockfile again as UTF-8 as if it were Latin-1, as writing it
    through Ada.Text_IO did. Twice is enough to produce C1 control characters.
    """
    with open(alr_lockfile(), "rb") as f:
        contents = f.read()
    for _ in range(times):
        contents = contents.decode("latin-1").encode("utf-8")
    with open(alr_lockfile(), "wb") as f:
        f.write(contents)


# A mangled lockfile is either still readable (mangled once), or regenerated
# with a warning (when it has control characters)

for times in [1, 2]:
    mangle(times)
    p = run_alr("show", quiet=False)
    if times > 1:
        assert_match(".*Warning: This workspace was created with a previous alr"
                     " version", p.out)

    run_alr("update")
    with open(alr_lockfile(), encoding="utf-8") as f:
        lockfile = f.read()
    assert_substring("C\u00e9sar <cesar@example.com>", lockfile)

print("SUCCESS")
