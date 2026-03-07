"""
Check that offered default licenses are all valid
"""

import os
import shutil

from drivers.alr import run_alr, run_alr_interactive

# iterate over values 1..8
for i in range(1, 9):

    # Run interactively
    run_alr_interactive(['init', '--bin', 'xxx'],
                        output=['> ' for _ in range(7)],
                        input=['',      # Description
                               '',      # Full user name
                               '',      # Github login
                               '',      # Email
                               f'{i}',  # License
                               '',      # Tags
                               ''],     # Website
                        timeout=3)

    # Check that it can be shown, which will load the manifest
    os.chdir("xxx")
    p = run_alr("show")

    # Prepare for next iteration
    os.chdir("..")
    shutil.rmtree("xxx")

print('SUCCESS')
