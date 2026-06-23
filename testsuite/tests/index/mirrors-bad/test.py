"""
Check that invalid mirrors are rejected at load time with a checked error.
"""

from drivers.alr import run_alr
from drivers.asserts import assert_match


def show_fails(crate, error):
    p = run_alr("show", crate,
                complain_on_error=False, debug=False, quiet=False)
    assert_match(".*ERROR:.*" + error + ".*", p.out)


# A mirror cannot carry identity fields; they are taken from the origin
show_fails("bad_commit", "mirrors cannot specify a commit")
show_fails("bad_hash", "mirrors cannot specify hashes")
show_fails("bad_subdir", "mirrors cannot specify a subdir")

# A mirror that resolves to a different origin kind is rejected
show_fails("bad_kind", "mirror is a .* but the authoritative origin is")

# Origin kinds that do not support mirrors are rejected
show_fails("bad_origin", "cannot have mirrors")


print("SUCCESS")
