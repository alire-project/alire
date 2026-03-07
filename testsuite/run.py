#! /usr/bin/env python

"""
e3.testsuite-based testsuite for Alire/ALR.

Just execute this script to run the testsuite. It requires a Python 3
interpreter with the e3-core and e3-testsuite packages (from PyPI) installed.
"""

from __future__ import absolute_import, print_function

import os.path
import shutil
import subprocess
import sys
from argparse import ArgumentTypeError

import e3.testsuite
import e3.testsuite.driver
from drivers.driver.docker_wrapper import DockerWrapperDriver
from drivers.driver.python_script import PythonScriptDriver
from drivers.helpers import on_windows


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = 'tests'

    # Available drivers
    test_driver_map = {
        'python-script'  : PythonScriptDriver,
        'docker-wrapper' : DockerWrapperDriver
        }

    def add_options(self, parser):
        super().add_options(parser)
        parser.add_argument('--alr', type=self._alr_path, default=self._default_alr_path(),
            dest='alr_path', metavar='FILE', help='''Set `alr` binary to run the testsuite
            against. Defaults to `alr` from project's `bin` directory.''')

    def require_executable(self, name):
        path = shutil.which(name)
        if path is None:
            raise FileNotFoundError(f"{name} not found in PATH")
        else:
            print(f"Testsuite using {name} at {path} with version:", )
            print(subprocess.run([name, '--version'],
                                 stdout=subprocess.PIPE).stdout.decode())
            sys.stdout.flush()

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

        # Ensure toolchain is in scope, or err early instead of during tests
        # Locate gnat and gprbuild in path and report their location
        required_executables = ['gnat', 'gprbuild']
        for exe in required_executables:
            self.require_executable(exe)
        print()

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
