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


from drivers.python_script import PythonScriptDriver


class Testsuite(e3.testsuite.Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'python-script': PythonScriptDriver}

    @property
    def default_driver(self):
        return 'decoder'


if __name__ == '__main__':
    suite = Testsuite(os.path.dirname(__file__))
    suite.testsuite_main()

    # Display statistics about test results: number of tests per status
    stats = [(str(name).split('.')[1], count)
             for name, count in suite.test_status_counters.items()
             if count]
    for name, count in sorted(stats):
        print('{: <8} {}'.format(name + ':', count))

    # Exit with failure if some test didn't pass
    for name, count in sorted(stats):
        if name == 'FAIL' and count > 0:
            exit(1)
