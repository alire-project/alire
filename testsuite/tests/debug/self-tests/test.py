"""
Do internal self-tests that are simpler to do in Ada code than with a fully-
fledged test case.
"""

from glob import glob

from drivers.alr import run_alr

p = run_alr('dev', '--test', complain_on_error=True, quiet=True)
assert p.status == 0, "alr should have error code 0"


print('SUCCESS')
