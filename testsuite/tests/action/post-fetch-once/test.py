"""
Test that post-fetch actions are invoked only once for the root crate during an
`alr get`
"""

from drivers.alr import run_alr

# Should silently retrieve everything
p = run_alr('get', 'main')

checks = 0
for line in p.out.splitlines():
    if "POST-FETCH TRIGGERED" in line:
        checks += 1

assert checks == 1, \
    f"Expected matching once but got: {checks} in output: {p.out}"

print('SUCCESS')
