#! /usr/bin/env python

"""
e3.testsuite-based testsuite for Alire/ALR.

Just execute this script to run the testsuite. It requires a Python 3
interpreter with the e3-core and e3-testsuite packages (from PyPI) installed.
"""

from __future__ import absolute_import, print_function

from argparse import ArgumentTypeError
import sys
import os.path

import e3.testsuite
import e3.testsuite.driver
from e3.testsuite.result import TestStatus

from drivers.helpers import on_windows
from drivers.driver.python_script import PythonScriptDriver
from drivers.driver.docker_wrapper import DockerWrapperDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {
        'python-script': PythonScriptDriver,
        'docker-wrapper': DockerWrapperDriver
        }

    def add_options(self, parser):
        super().add_options(parser)
        parser.add_argument('--alr', type=self._alr_path, default=self._default_alr_path(),
            dest='alr_path', metavar='FILE', help='''Set `alr` binary to run the testsuite
            against. Defaults to `alr` from project's `bin` directory.''')

    def set_up(self):
        super().set_up()
        os.environ['ALR_PATH'] = self.main.args.alr_path

        # Some alr commands spawn another `alr` which must be found in path.
        # This way we ensure the same alr being tested is used.
        os.environ["PATH"] = \
            f"{os.path.dirname(self.main.args.alr_path)}{os.pathsep}{os.environ['PATH']}"

        # Some tests rely on an initially empty GPR_PROJECT_PATH variable
        os.environ.pop('GPR_PROJECT_PATH', None)

        # Define a flag so that we don't run potentially dangerous actions
        # during the tests (e.g. submitting a release by accident)
        os.environ["ALR_TESTSUITE"] = "TRUE"

    def _alr_path(self, alr_file):
        alr_path = os.path.abspath(alr_file)
        if not os.path.isfile(alr_path):
            raise ArgumentTypeError(fr"'{alr_file}' is not a file or does not exist")
        return alr_path

    def _default_alr_path(self):
        project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        return os.path.join(project_root, 'bin', f"alr{'.exe' if on_windows() else ''}")


if __name__ == '__main__':
    suite = Testsuite()
    sys.exit(suite.testsuite_main(sys.argv[1:] + ["--failure-exit-code=1"]))
