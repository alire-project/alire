"""
Verify that pinning a bad path fails
"""


from drivers.alr import run_alr, init_local_crate
from drivers.asserts import  assert_match


# Pin a local path which doesn't exist
init_local_crate()
p = run_alr("with", "badcrate", "--use", "../bad/path", complain_on_error=False)
assert_match(
    r".*Pin path is not a valid directory: .*/bad/path",
    p.out
)

# Verify that a warning is issued for a path that looks like a remote URL.
p = run_alr(
    "with", "badcrate", "--use", "https://some.host/path",
    quiet=False, complain_on_error=False
)
assert_match(
    (
        r".*Assuming 'https://some\.host/path' is a directory because no "
        r"branch or commit was specified\."
    ),
    p.out
)
assert_match(
    r".*Pin path is not a valid directory: .*/xxx/https:/some.host/path",
    p.out
)


print('SUCCESS')
