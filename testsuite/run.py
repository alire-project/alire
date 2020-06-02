#! /usr/bin/env python

"""
e3.testsuite-based testsuite for Alire/ALR.

Just execute this script to run the testsuite. It requires a Python2
interpreter with the e3-core and e3-testsuite packages (from PyPI) installed.
"""

from __future__ import absolute_import, print_function

import os.path

import e3.testsuite
import e3.testsuite.driver
from e3.testsuite.result import TestStatus


from drivers.python_script import PythonScriptDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {'python-script': PythonScriptDriver}


if __name__ == '__main__':
    suite = Testsuite()
    suite.testsuite_main()

    # Exit with failure if some test didn't pass
    for name, count in suite.test_status_counters.items():
        if count > 0 and name not in (
            TestStatus.PASS, TestStatus.XFAIL, TestStatus.XPASS,
            TestStatus.SKIP
        ):
            exit(1)
