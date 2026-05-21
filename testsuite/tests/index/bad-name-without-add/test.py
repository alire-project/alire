'''
Check that specifying --name without --add is properly reported.
'''

from drivers.alr import run_alr
from drivers.asserts import assert_eq


# --name without --add should fail, without first listing indexes.
p = run_alr('index', '--name', 'test', complain_on_error=False, debug=False)
assert_eq('ERROR: --name is only valid with --add\n', p.out, show_escaped=True)

# --name with --del should fail, without attempting deletion or listing indexes.
p = run_alr('index', '--name', 'test', '--del', 'existing',
            complain_on_error=False, debug=False)
assert_eq('ERROR: --name is only valid with --add, not with --del\n', p.out,
          show_escaped=True)

print('SUCCESS')
