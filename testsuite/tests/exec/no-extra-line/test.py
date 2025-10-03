"""
Check that `alr exec` does not add an extra line at the end of the output.
Fix for issue #2004. This test is more or less moot, since the bug can only be
observed when running interactively with ANSI coloring, which is disabled in
the testsuite and cannot be forced on. At least, it checks that no extra line is
added in the non-interactive case.
"""

from drivers.alr import run_alr, init_local_crate
from drivers.asserts import assert_eq

# Initialize a crate, enter it, and run an echo through exec. Check the output.

init_local_crate()

p = run_alr("exec", "--", "echo", "HELLO", quiet=False)
assert_eq("HELLO\n", p.out)  # Note: only one newline at the end

print("SUCCESS")
