import copy
import os
import shutil
import sys
import time

from drivers.alr import prepare_env, prepare_indexes, run_alr
from drivers.driver.base_driver import BaseDriver

from e3.testsuite.driver.classic import (TestAbortWithFailure,
                                         TestSkip)


class PythonScriptDriver(BaseDriver):
    """
    Test driver to run a "test.py" Python script.

    This test driver runs a Python script. For a testcase to succeeds, the
    script expects it to exit with status code 0, its standard error stream to
    be empty and its standard output stream to end with a line that contains
    "SUCCESS". If a test must be skipped, it should print "SKIP: <reason>".
    Anything else results in the test failing.
    """

    # This is a workaround for Windows, where attempting to use rlimit by e3-core
    # causes permission errors. TODO: remove once e3-core has a proper solution.
    @property
    def default_process_timeout(self):
        return None

    def prepare(self) -> dict:
        # prepare a private environment for Python scripts to run "alr".

        env = dict(os.environ)

        # disable traceback from parent environment if it existed
        env.pop('ALR_TRACEBACK_ENABLED', None)

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

        return env


    def run_script(self, env):
        # Run the Python script with the current interpreter. check_call aborts
        # the test if the interpreter exits with non-zero status code.
        return self.shell([sys.executable, 'test.py'],
                          env=env,
                          cwd=self.test_env['working_dir'])


    def check_result(self, p):
        # Check that the test output is proper (no missing status)
        out_lines = p.out.splitlines()
        if out_lines and out_lines[-1] == 'SUCCESS':
            pass
        elif out_lines and (reason := out_lines[-1]).startswith('SKIP:'):
            raise TestSkip(reason.split(":")[-1].strip())
        else:
            self.result.log += 'missing SUCCESS output line'
            raise TestAbortWithFailure('missing SUCCESS output line')


    def save_working_dir(self):
        # Save the working directory state for later restoration.

        base = self.test_env['working_dir']
        orig_name = ".orig"
        orig = os.path.join(base, orig_name)

        # Save the original files under ".orig" folder
        os.mkdir(orig)
        for f in os.listdir(base):
            if f == orig_name:
                continue
            path = os.path.join(base, f)
            if os.path.isfile(path):
                shutil.copy(path, orig)
            else:
                shutil.copytree(path, os.path.join(orig, f))


    def restore_working_dir(self):
        # Restore the working directory to its initial state, by deleting
        # everything and copying originals back from .orig dir

        def make_writable(path):
            # Make everything inside a directory writable recursively
            for root, dirs, files in os.walk(path):
                for d in dirs:
                    os.chmod(os.path.join(root, d), 0o777)
                for f in files:
                    os.chmod(os.path.join(root, f), 0o666)

        base = self.test_env['working_dir']
        orig_name = ".orig"

        # Delete anything not called ".orig"
        for f in os.listdir(base):
            if f != orig_name:
                path = os.path.join(base, f)
                if os.path.isfile(path):
                    os.remove(path)
                else:
                    # Git marks some files read-only, so make them writable
                    make_writable(path)
                    shutil.rmtree(path)

        # Restore the original files
        orig = os.path.join(base, orig_name)
        for f in os.listdir(orig):
            path = os.path.join(orig, f)
            if os.path.isfile(path):
                shutil.copy(path, base)
            else:
                shutil.copytree(path, os.path.join(base, f))


    def run(self):
        # Run the test itself. Depending on the build mode, it may be run
        # twice.
        DEFAULT_MODE = "both"

        pristine_env = self.prepare()

        # Obtain the build mode for the test
        mode = self.test_env.get('build_mode',
                                 self.test_env.get('build-mode',
                                                   DEFAULT_MODE))
        # One of 'shared', 'sandboxed', or 'both'
        if mode not in ["shared", "sandboxed", "both"]:
            raise ValueError(f"Invalid build mode: {mode}, must be one of "
                             "'shared', 'sandboxed', or 'both'")

        # If mode is "both", track original files for later
        if mode == "both":
            self.save_working_dir()

        start_time = time.time()

        # First run with shared builds disabled

        if mode in ["sandboxed", "both"]:
            self.result.log.log += "Build mode: SANDBOXED\n"
            p = self.run_script(copy.deepcopy(pristine_env))
            self.check_result(p)

        # Second run with shared builds enabled

        # Start by cleaning up anything the 1st run may have left behind
        if mode == "both":
            self.restore_working_dir()

        if mode in ["shared", "both"]:
            self.result.log.log += "Build mode: SHARED\n"
            # Activate shared builds. Using "-c" is needed as the environment
            # still isn't activated at the driver script level.
            run_alr(f"--settings={pristine_env['ALIRE_SETTINGS_DIR']}",
                    "settings", "--global", "--set",
                    "dependencies.shared", "true")
            p = self.run_script(copy.deepcopy(pristine_env))
            self.check_result(p)

        self.result.time = time.time() - start_time
