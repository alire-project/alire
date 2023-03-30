"""
This is run inside a docker container, so we aren't in the context of e3. We
prepare the environment and run the test script directly.
"""

import json
import os
import subprocess
import sys

from drivers import alr
from drivers.alr import run_alr


def main():
    testsuite_root = "/testsuite"  # Must match docker volume mount
    home = os.environ["HOME"]
    work_dir = "/tmp/test"

    # We receive the test environment prepared by e3 as JSON via environment.
    # With this test_env dictionary we are mostly in the same situation as in
    # the Python driver.
    test_env = json.loads(os.environ['ALIRE_TEST_ENV'])

    # Find the test sources inside docker. The test_env we got still has the
    # host paths.

    test_dir = test_env['test_dir']  # The test source dir to find test.py
    # Strip the prefix that comes from the host filesystem
    test_dir = test_dir[test_dir.find(testsuite_root):]

    # Create a pristine folder for the test to run in
    os.mkdir(work_dir)

    # Set up the environment

    # alr path
    os.environ["ALR_PATH"] = "/usr/bin/alr" # Must match docker volume mount

    # Disable autoconfig of the community index, to prevent unintended use
    run_alr("config", "--global", "--set", "index.auto_community", "false")

    # Disable selection of toolchain. Tests that
    # require a configured compiler will have to set it up explicitly.
    run_alr("toolchain", "--disable-assistant")

    # Disable warning on old index, to avoid having to update index versions
    # when they're still compatible.
    run_alr("config", "--global", "--set", "warning.old_index", "false")

    # indexes to use
    if 'indexes' in test_env:
        alr.prepare_indexes(
            config_dir=home + "/.config/alire",
            working_dir=work_dir,
            index_descriptions=test_env.get('indexes', {}))

    # Give access to our Python helpers
    env = os.environ.copy()
    python_path = env.get('PYTHONPATH', '')
    env['PYTHONPATH'] = python_path  # Ensure it exists, even if empty
    env['PYTHONPATH'] += os.path.pathsep + testsuite_root  # And add ours

    # Run the test
    try:
        subprocess.run(["python3", test_dir + "/test.py"],
                       cwd=work_dir, env=env, check=True)
    except:
        # No need to let the exception to muddle the reporting, the output of
        # the test with any exception is already in the log that the
        # docker_wrapper will print.
        sys.exit(1)


if __name__ == '__main__':
    main()
