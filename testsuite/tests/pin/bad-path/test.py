"""
Verify that pinning a bad path is rejected
"""

from drivers.alr import run_alr, alr_pin, alr_unpin, init_local_crate
from drivers.asserts import assert_match

# Pin a local path which doesn't exist
init_local_crate()
alr_pin("badcrate", path="../bad/path", update=False)

# Now the update should detect the bad path
p = run_alr("update", complain_on_error=False)
assert_match(
    ".*Pin path is not a valid directory:.*",
    p.out
)

# Verify the same outcome with the command line interface
alr_unpin("badcrate")
p = run_alr("pin", "badcrate", "--use", "../bad/path")
assert_match(
    r".*Given path does not exist: \.\./bad/path",
    p.out
)
# Note that `alr` returns zero because the confirmation prompt abandons the
# operation by default.
assert_match(
    r".*Do you want to continue anyway\?",
    p.out
)
assert_match(
    r".*Using default: No",
    p.out
)

# Verify the same result with a `file:` URL.
p = run_alr("pin", "badcrate", "--use", "file:../bad/path")
assert_match(
    r".*Given path does not exist: \.\./bad/path",
    p.out
)

# Verify that a warning is issued for a path that looks like a remote URL.
p = run_alr("pin", "badcrate", "--use", "https://some.host/path", quiet=False)
assert_match(
    (
        r".*Assuming 'https://some\.host/path' is a directory because no "
        r"branch or commit was specified\."
    ),
    p.out
)
assert_match(
    r".*Given path does not exist: https://some\.host/path",
    p.out
)

print('SUCCESS')
