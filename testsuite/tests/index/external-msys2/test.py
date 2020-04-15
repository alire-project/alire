"""
Test that installing msys2 dependency work as expected.
"""

from drivers.alr import run_alr

import platform

if platform.system() == 'Windows':
    # Should silently retrieve everything
    p = run_alr('--non-interactive', '-v', 'get', 'main', quiet=False, debug=True)
    
    checks = 0
    for line in p.out.splitlines():
        if line.startswith("cdialog (ComeOn Dialog!) version "):
            print("dialog output matched")
            checks += 1
    
    assert checks == 1, 'Only %d match in the output : "%s"' % (checks, p.out)

print('SUCCESS')
    