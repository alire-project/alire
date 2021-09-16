"""
Test that -d/--debug enable dumping of unexpected exceptions
"""

import os

from drivers.alr import prepare_env, prepare_indexes, run_alr
from drivers.asserts import assert_eq, assert_match


def check_output(dump):
    assert_match(
	'''stderr: PROGRAM_ERROR
stderr: Raising forcibly
stderr: raised PROGRAM_ERROR : Raising forcibly.*
''', dump)


# Check debug dump (we intentionally disable debug flag in run_alr)
check_output(run_alr('-d', 'dev', '--raise',
                     debug=False, complain_on_error=False).out)
# Long flag version
check_output(run_alr('--debug', 'dev', '--raise',
                     debug=False, complain_on_error=False).out)

# Check ordinary non-debug output:
assert_eq(run_alr('dev', '--raise', debug=False, complain_on_error=False).out,
          "ERROR: Raising forcibly\n"
          "ERROR: alr encountered an unexpected error, re-run with -d for details.\n")

print('SUCCESS')
