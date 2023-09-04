"""
Test that installing msys2 dependency work as expected. The existence of the
'dialog' command is checked in a post-fetch action.
"""

import os
import platform
from glob import glob

from drivers.alr import run_alr

if platform.system() == 'Windows':
    # Should silently retrieve everything
    run_alr('get', 'main')
    os.chdir(glob('main*')[0])

    # Trigger post-fetch
    p = run_alr('build', 'main',
                quiet=False, debug=True, complain_on_error=False)

    checks = 0
    for line in p.out.splitlines():
        if line.startswith("cdialog (ComeOn Dialog!) version "):
            print("dialog output matched")
            checks += 1

    assert checks == 1, 'Only %d match in the output : "%s"' % (checks, p.out)

    print('SUCCESS')

else:
    print('SKIP: test is Windows-only')
