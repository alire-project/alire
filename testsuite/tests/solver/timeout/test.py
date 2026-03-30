"""
Test solver timeout behaviors
"""

import re
from drivers.alr import alr_settings_set, alr_with, init_local_crate, run_alr, run_alr_interactive
from drivers.asserts import assert_not_substring, assert_substring, assert_match

TELLTALE = "SOLVER: search timeout"
"""The debug message issued when the solver times out."""
DIFF_SUMMARY_MSG = "New solution is incomplete (timed out)."
"""The summary heading for solution diffs when incomplete due to timeout."""
SHOW_WARNING = "Warning: Dependency resolution timed out"
"""The warning issued by `alr show` when incomplete due to timeout."""
UPDATE_NO_CHANGES_WARNING = (
    "Warning: Dependency resolution timed out with missing dependencies "
    "(use `alr with --solve` for details).\nNothing to update."
)
"""
The warning `alr update` issues when the solution is incomplete due to a timeout
but there is no diff to confirm.
"""

# Configure solver for immediate timeouts
alr_settings_set("solver.timeout", "0")
alr_settings_set("solver.grace_period", "0")

# Configure solver to never find a solution in order to force timeouts
alr_settings_set("solver.never_finish", "true")

# First, check a regular timeout does happen for non-interactive execution of
# `alr show --solve`, with an appropriate warning and a zero exit code.
assert_substring(TELLTALE, run_alr("-vv", "show", "--solve", "hello", quiet=False).out)
assert_substring(SHOW_WARNING, run_alr("show", "--solve", "hello", quiet=False).out)

# Check that `alr get` and `alr install` time out with a non-zero exit code.
for subcmd in ("get", "install"):
    p_verbose = run_alr("-vv", subcmd, "hello", quiet=False, complain_on_error=False)
    assert_substring(TELLTALE, p_verbose.out)
    p = run_alr(subcmd, "hello", quiet=False, complain_on_error=False)
    assert_substring("Timed out before finding a complete solution for hello", p.out)
    if subcmd == "get":
        assert_substring(DIFF_SUMMARY_MSG, p.out)

# Check that plain `search` never asks, as no solving is happening
assert_not_substring(TELLTALE, run_alr("-vv", "search", "hello", quiet=False).out)

# Check that `search --solve` does not ask even in interactive mode, as it does
# a best-effort solving. If there were unexpected interactivity, we would get a
# RuntimeError here. Still, there is an internal solver timeout that we forced.
search_output = run_alr_interactive(["-vv", "search", "hello", "--solve"], [], [])
assert_substring(TELLTALE, search_output)
# The crate's status should be `?`.
assert_match(r".*\nhello\s+\?\s+1\.0\.1", search_output)

# Enter a new crate to test commands that require one.
init_local_crate()

# Check that the timeout is noted in the diff print for non-interactive `with`,
# `pin`, and `update`, with confirmation defaulting to No and no changes applied.
assert_substring(DIFF_SUMMARY_MSG, run_alr("with", "hello", quiet=False).out)
alr_with("hello", manual=True, update=False)
assert_substring(DIFF_SUMMARY_MSG, run_alr("with", quiet=False).out)
assert_substring(DIFF_SUMMARY_MSG, run_alr("pin", "hello=1.0.0", quiet=False).out)
assert_substring(UPDATE_NO_CHANGES_WARNING, run_alr("update", quiet=False).out)


# Test interactive timeout prompts.

SHOW_WARNING_RE = ".*" + re.escape(SHOW_WARNING)

SOLVER_ASKS = ".*" + re.escape("[Y] Yes  [N] No  [A] Always  (default is Yes)") + r"\s*$"
SOLVER_ASKS_INCOMPLETE = (
    ".*" + re.escape("Warning: Complete solution not found after 0 seconds.") + SOLVER_ASKS
)
SOLVER_ASKS_INEXHAUSTIVE = (
    ".*" + re.escape("Warning: Solution space not fully explored after 0 seconds.") + SOLVER_ASKS
)

CONFIRM_DIFF = r".*\[Y\] Yes  \[N\] No  \(default is {default}\)\s*$"
CONFIRM_DIFF_INCOMPLETE = (
    ".*" + re.escape(DIFF_SUMMARY_MSG) + CONFIRM_DIFF.format(default="No")
)
CONFIRM_DIFF_COMPLETE = (
    ".*" + re.escape("New solution is complete.") + CONFIRM_DIFF.format(default="Yes")
)

for cmd, output, input in [
    # `show` with exit on 1st timeout: only trivial solution found, so warns
    # it's incomplete
    (["show", "--solve", "hello"], [SOLVER_ASKS_INCOMPLETE, SHOW_WARNING_RE], ["n"]),
    (["show", "--tree", "hello"], [SOLVER_ASKS_INCOMPLETE, SHOW_WARNING_RE], ["n"]),
    # `show` with exit on 2nd timeout: complete (albeit not exhaustive) solution
    # found, so no warning
    (
        ["show", "--solve", "hello"],
        [SOLVER_ASKS_INCOMPLETE, SOLVER_ASKS_INEXHAUSTIVE, rf"(?!{SHOW_WARNING_RE})"],
        ["y", "n"],
    ),
    # `update` with exit on 1st timeout: only trivial solution found, so no
    # changes to confirm
    (["update"], [SOLVER_ASKS_INCOMPLETE, ".*" + re.escape(UPDATE_NO_CHANGES_WARNING)], ["n"]),
    # `update` with exit on 2nd timeout: solved for `hello` but not `libhello`,
    # so asks for confirmation of incomplete solution
    (
        ["update"],
        [SOLVER_ASKS_INCOMPLETE] * 2 + [CONFIRM_DIFF_INCOMPLETE],
        ["y", "n", "n"],
    ),
    # `update` with exit on 3rd timeout: complete (albeit not exhaustive), so
    # changes confirmation defaults to yes
    (
        ["update"],
        [SOLVER_ASKS_INCOMPLETE] * 2 + [SOLVER_ASKS_INEXHAUSTIVE, CONFIRM_DIFF_COMPLETE],
        ["y", "y", "n", "n"],
    ),
    # `with` with exit on 1st timeout: only trivial solution found, so asks for
    # confirmation of incomplete solution
    (["with", "libhello"], [SOLVER_ASKS_INCOMPLETE, CONFIRM_DIFF_INCOMPLETE], ["n", "n"]),
    # `with` with exit on 3rd timeout: complete (albeit not exhaustive), so
    # changes confirmation defaults to yes
    (
        ["with", "libhello"],
        [SOLVER_ASKS_INCOMPLETE] * 2 + [SOLVER_ASKS_INEXHAUSTIVE, CONFIRM_DIFF_COMPLETE],
        ["y", "y", "n", "n"],
    ),
]:
    tail = run_alr_interactive(cmd, output, input)
    if len(output) > len(input):
        assert_match(output[len(input)], tail)


print("SUCCESS")
