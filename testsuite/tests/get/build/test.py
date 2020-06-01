"""
Check the `alr get --build` combo for crates that [don't] build
"""

from drivers.alr import run_alr

# Should succeed
p = run_alr('get', '--build', 'good')

# Should err out
p = run_alr('get', '--build', 'bad',
            complain_on_error=False)
assert p.status == 1, "Expected command to exit with code 1"


print('SUCCESS')
