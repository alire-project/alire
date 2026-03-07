"""
Validate tricky user inputs during crate initialization
"""

import os
import shutil

from drivers.alr import run_alr, run_alr_interactive
from drivers.asserts import assert_eq


def check(descr: str = "", name: str = ""):
    """
    Initialize a crate with given description and name
    """
    run_alr_interactive(['init', '--bin', 'xxx'],
                        output=['> ' for _ in range(7)],
                        input=[descr,   # Description
                               name,    # Full user name
                               '',      # Github login
                               '',      # Email
                               '',      # License
                               '',      # Tags
                               ''],     # Website
                        timeout=3)

    # Check that it can be shown, which will load the manifest
    os.chdir("xxx")
    p = run_alr("show")

    # Verify the description and author in output match the input

    # Description is in first line after the ": "
    assert_eq(p.out.splitlines()[0].split(": ", 1)[1], descr)

    # Author is in the fourth line after the ": "
    assert_eq(p.out.splitlines()[3].split(": ", 1)[1], name)

    # Prepare for next iteration
    os.chdir("..")
    shutil.rmtree("xxx")

    # Unset the user information so it is re-asked next time
    run_alr('settings', '--global', '--unset', 'user.name')
    # Other fields are not being set because they use the defaults so don't
    # require unsetting.


# Try a few tricky inputs
check('hello "world"', 'Robert "Bobby" Tables')
check('"hello" "world"', '"Bobby" Robert Tables')
check('""""""', '"Bobby" Robert "Tables"')


print('SUCCESS')
