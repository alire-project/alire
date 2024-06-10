"""
Test that post-fetch actions are not invoked for the root crate during an
`alr get`, as post-fetch is run during first build when config is complete.
"""

from drivers.alr import run_alr

# Should silently retrieve everything
p = run_alr('get', 'main')

checks = 0
for line in p.out.splitlines():
    if "POST-FETCH TRIGGERED" in line:
        checks += 1

assert checks == 0, \
    f"Expected matching once but got: {checks} in output: {p.out}"

print('SUCCESS')
