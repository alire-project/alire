"""
Test that -d/--debug enable dumping of unexpected exceptions
"""

import os

from drivers.alr import prepare_env, prepare_indexes, run_alr
from drivers.asserts import assert_eq, assert_match


def check_output(dump):
    assert_match('''\
.*
ERROR: Location  : .*
ERROR: Extra info: Raising forcibly
ERROR: Report at : .*
ERROR: Re-run with global switches `-vv -d` for a full log and stack trace.\
''', dump)


# Check debug dump (we intentionally disable debug flag in run_alr)
check_output(run_alr('-d', 'dev', '--raise',
                     debug=False, complain_on_error=False).out)
# Long flag version
check_output(run_alr('--debug', 'dev', '--raise',
                     debug=False, complain_on_error=False).out)

# Check ordinary non-debug output:
assert_match(
          ".*"
          "ERROR: Extra info: Raising forcibly.*"
          "ERROR: Re-run with global switches `-vv -d` for a full log and stack trace",
          run_alr('dev', '--raise', debug=False, complain_on_error=False).out
          )

# Check exception from finalization :
assert_match(
          ".*"
          "ERROR: Raising forcibly from finalization\n"
          "ERROR: alr encountered an unexpected error, re-run with -d for details.\n"
          "ERROR: error location: .*",
          run_alr('dev', '--raise-finalization', debug=False, complain_on_error=False).out)

print('SUCCESS')
