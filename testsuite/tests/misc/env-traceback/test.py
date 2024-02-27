"""
Check ALR_TRACEBACK_ENABLED env var
"""

import os
from drivers.alr import run_alr
from drivers.asserts import assert_eq, assert_match

def check_no_traceback():
    assert_match("""\
ERROR: .* BEGIN Alire bug detected .*
ERROR: Location  : Alr.Commands.Dev.Execute at alr-commands-dev.adb:.*
ERROR: Extra info: Raising forcibly
ERROR: Report at : .*
ERROR: Re-run with `-vv -d` for a full log and stack trace.
ERROR: .* END Alire bug detected .*\
""",
              run_alr("dev", "--raise",
                      debug=False, complain_on_error=False).out)


def check_traceback():
    assert_match(".*0x", # appears in both symbolic and raw tracebacks
                 run_alr("dev", "--raise",
                         debug=False, complain_on_error=False).out)


# By default (no `-d` or ALR_TRACEBACK_ENABLED) we don't get a backtrace

check_no_traceback()

# Explicit disable

for val in ["", "0", "false", "no"]:
    os.environ['ALR_TRACEBACK_ENABLED'] = val
    check_no_traceback()

# With ALR_TRACEBACK_ENABLED we do get a backtrace

for val in ["1", "true", "yes"]:
    os.environ['ALR_TRACEBACK_ENABLED'] = val
    check_traceback()

print('SUCCESS')
