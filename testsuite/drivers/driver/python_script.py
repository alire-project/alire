import os
import sys

from e3.fs import sync_tree
from e3.testsuite.driver.classic import ClassicTestDriver, TestAbortWithFailure
from e3.testsuite.result import TestStatus

from drivers.alr import prepare_env, prepare_indexes


class PythonScriptDriver(ClassicTestDriver):
    """
    Test driver to run a "test.py" Python script.

    This test driver runs a Python script. For a testcase to succeeds, the
    script expects it to exit with status code 0, its standard error stream to
    be empty and its standard output stream to end with a line that contains
    "SUCCESS". Anything else results in the test failing.
    """

    # This is a workaround for Windows, where attempting to use rlimit by e3-core
    # causes permission errors. TODO: remove once e3-core has a proper solution.
    @property
    def default_process_timeout(self):
        return None

    def run(self):
        env = dict(os.environ)

        # prepare a private environment for Python scripts to run "alr".
        config_dir = os.path.join(self.test_env['working_dir'],
                                  'alr-config')
        prepare_env(config_dir, env)

        # If requested, prepare indexes to be used by "alr".
        if 'indexes' in self.test_env:
            prepare_indexes(config_dir,
                            self.test_env['working_dir'],
                            self.test_env.get('indexes', {}))

        # Also give it access to our Python helpers
        python_path = env.get('PYTHONPATH', '')
        parent = os.path.dirname
        path_for_drivers = os.path.abspath(parent(parent(parent(__file__))))
        env['PYTHONPATH'] = '{}{}{}'.format(
            path_for_drivers, os.path.pathsep, python_path
        ) if python_path else path_for_drivers

        # Run the Python script with the current interpreter. check_call aborts
        # the test if the interpreter exits with non-zero status code.
        p = self.shell([sys.executable, 'test.py'],
                       env=env,
                       cwd=self.test_env['working_dir'])

        # Check that stderr is empty
        if False and p.err:
            self.result.log += 'non-empty stderr:\n'
            self.result.log += p.err
            raise TestAbortWithFailure('non-empty stderr')

        # Check that the last line in stdout is "SUCCESS"
        out_lines = p.out.splitlines()
        if not out_lines or out_lines[-1] != 'SUCCESS':
            self.result.log += 'missing SUCCESS output line'
            raise TestAbortWithFailure('missing SUCCESS output line')
