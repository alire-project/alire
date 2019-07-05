import os
import sys

from e3.fs import sync_tree
from e3.testsuite import TestAbort
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus

from drivers.alr import prepare_env, prepare_indexes


class PythonScriptDriver(TestDriver):
    """
    Test driver to run a "test.py" Python script.

    This test driver runs a Python script. For a testcase to succeeds, the
    script expects it to exit with status code 0, its standard error stream to
    be empty and its standard output stream to end with a line that contains
    "SUCCESS". Anything else results in the test failing.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'run')

    def run(self, previous_values):
        # Copy test material to the temporary directory
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

        env = dict(os.environ)

        # If requested, prepare a default environment for Python scripts to
        # run "alr".
        if 'indexes' in self.test_env:
            config_dir = os.path.join(self.test_env['working_dir'],
                                      'alr-config')
            prepare_env(config_dir, env)
            prepare_indexes(config_dir,
                            self.test_env['working_dir'],
                            self.test_env.get('indexes', {}))

        # Also give it access to our Python helpers
        python_path = env.get('PYTHONPATH', '')
        path_for_drivers = os.path.abspath(
            os.path.dirname(os.path.dirname(__file__)))
        env['PYTHONPATH'] = '{}{}{}'.format(
            path_for_drivers, os.path.pathsep, python_path
        ) if python_path else path_for_drivers

        # Run the Python script with the current interpreter. check_call aborts
        # the test if the interpreter exits with non-zero status code.
        p = check_call(self,
                       [sys.executable, 'test.py'],
                       env=env,
                       cwd=self.test_env['working_dir'])

        # Check that stderr is empty
        if p.err:
            self.result.set_status(TestStatus.FAIL, 'non-empty stderr')
            self.result.log += 'non-empty stderr:\n'
            self.result.log += p.err
            self.push_result()
            raise TestAbort

        # Check that the last line in stdout is "SUCCESS"
        out_lines = p.out.splitlines()
        if not out_lines or out_lines[-1] != 'SUCCESS':
            self.result.set_status(TestStatus.FAIL,
                                   'missing SUCCESS output line')
            self.result.log += 'missing SUCCESS output line'
            self.push_result()
            raise TestAbort

        # If we reach this, the test succeeds
        self.result.set_status(TestStatus.PASS)
        self.push_result()
        return True
