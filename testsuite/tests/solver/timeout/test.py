"""
Test solver timeout behaviors
"""

import re
from drivers.alr import alr_settings_set, alr_with, init_local_crate, run_alr, run_alr_interactive
from drivers.asserts import assert_not_substring, assert_substring

TELLTALE = "SOLVER: search timeout"

# Configure solver for immediate timeouts
alr_settings_set("solver.timeout", "0")
alr_settings_set("solver.grace_period", "0")

# Configure solver to never find a solution in order to force timeouts
alr_settings_set("solver.never_finish", "true")

# First, check a regular timeout does happen for non-interactive execution,
# which completes as expected.
assert_substring(TELLTALE,
                 run_alr("-vv", "show", "--solve", "hello", quiet=False).out)

# Check that plain `search` never asks, as no solving is happening
assert_not_substring(TELLTALE,
                     run_alr("-vv", "search", "hello", quiet=False).out)

# Check that `search --solve` does not ask even in interactive mode, as it does
# a best-effort solving. If there were unexpected interactivity, we would get a
# RuntimeError here. Still, there is an internal solver timeout that we forced.
assert_substring(\
    TELLTALE,
    run_alr_interactive(["-vv", "search", "hello", "--solve"], [], []))

# Check other commands that should have interactive timeouts. Enter a crate
# beforehand to take advantage of commands that require one.

init_local_crate()
alr_with("hello")

SOLVER_ASKS=re.compile(".*" + \
                       re.escape("[Y] Yes  [N] No  [A] Always  (default is Yes)") + \
                       "\s*$")
WITH_ASKS=re.compile(".*" + \
                       re.escape("[Y] Yes  [N] No  (default is No)") + \
                       "\s*$")

for cmd, output, input in\
      [(["show", "--solve", "hello"], [SOLVER_ASKS], ["n"]),                   # Exit on 1st timeout
       (["show", "--solve", "hello"], [SOLVER_ASKS, SOLVER_ASKS], ["y", "n"]), # Exit on 2nd timeout
       (["show", "--tree", "hello"],  [SOLVER_ASKS], ["n"]),
       (["update"],                   [SOLVER_ASKS], ["n"]),
       (["with", "libhello"],         [SOLVER_ASKS, WITH_ASKS], ["n", "n"])
       # First question is the solver asking for more time to find a solution, 
       # second is the user being asked to accept an incomplete solution.
      ]:
    run_alr_interactive(cmd, output, input)
                     

print("SUCCESS")
