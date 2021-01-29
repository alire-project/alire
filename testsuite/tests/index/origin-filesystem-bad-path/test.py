"""
Test that invalid filesystem origins are reported.
"""

import os

from drivers.alr import prepare_env, prepare_indexes, run_alr
from drivers.asserts import assert_match


def run(i, error):
    config_dir = 'alr-config-{}'.format(i)
    prepare_env(config_dir, os.environ)
    prepare_indexes(
        config_dir, '.', {'bad_index_{}'.format(i): {'in_fixtures': False}})
    p = run_alr("search", "--crates", complain_on_error=False, debug=False)
    assert_match(
        'ERROR: {}\n'
        'ERROR: alr encountered an unexpected error,'
        ' re-run with -d for details.\n$'.format(error),
        p.out)


run(1, '.*empty path given in local origin')

# Since the location reported is an absolute path, and thus filesystem
# dependent, check only that the beginning of the error is there:
run(2, 'Local origin path is not a valid directory: .*non-existing-path')

print('SUCCESS')
