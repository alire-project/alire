"""
Test the fidelity of various logging scopes
"""

from glob import glob

from drivers.alr import run_alr
from drivers.asserts import assert_eq
from drivers.asserts import assert_match

# Check invalid syntax detection
p = run_alr('-d--', complain_on_error=False, quiet=False)
assert p.status == 1, "alr should have error code 1"
assert_eq('Filtering mode: BLACKLIST\n'
          'Invalid logging scope separator: --\n'
          'ERROR: Invalid logging filters.',
          p.out.strip())

# Check empty whitelist lets nothing through
p = run_alr('-vv', '-d+', 'dev', quiet=False)
assert_eq('Filtering mode: WHITELIST', p.out.strip())

# Check whitelisting
p = run_alr('-vv', '-d+dev', 'dev', '--filter', quiet=False)
assert_match('Filtering mode: WHITELIST\n'
             'Filtering substring: dev\n'
             '\[Alr.Commands.Dev.Execute\] \(alr-commands-dev.adb:[0-9]* \)'
             ' -->> In dev --filter',
             p.out.strip())

# Check whitelisting with exception
p = run_alr('-vv', '-d+dev-exec', 'dev', '--filter', quiet=False)
assert_eq('Filtering mode: WHITELIST\n'
          'Filtering substring: dev\n'
          'Filtering exception: exec',
          p.out.strip())

# Check blacklisting with exception
p = run_alr('-vv', '-d-a+main', 'dev', quiet=False)
assert_match('Filtering mode: BLACKLIST\n'
             'Filtering substring: a\n'
             'Filtering exception: main\n'
             '\[Alr.Main                \] \(alr-main.adb:.*',
             # Remaining output is likely to change in the future so lets just
             # stop here the matching.
             p.out.strip())

print('SUCCESS')
