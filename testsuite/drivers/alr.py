"""
Helpers to run alr in the testsuite.
"""

import os.path

from e3.os.process import Run, quote_arg
from e3.fs import mkdir


TESTSUITE_ROOT = os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))


class CalledProcessError(Exception):
    pass


def prepare_env(config_dir, env):
    """
    Prepare the environment to run "alr".

    This creates the `config_dir` directory and updates `env` (environment
    variables) to point to it as alr's configuration directory.
    """
    config_dir = os.path.abspath(config_dir)
    mkdir(config_dir)
    env['ALR_CONFIG'] = config_dir


def run_alr(*args, **kwargs):
    """
    Run "alr" with the given arguments.

    :param bool complain_on_error: If true and the subprocess exits with a
        non-zero status code, print information on the standard output (for
        debugging) and raise a CalledProcessError (to abort the test).
    :param bool quiet: If true (which is the default), append "-q" to the
        command line.
    :param bool debug: If true (which is the default), append "-d" to the
        command line. This ensures uncaught exceptions are logged instead
        of presenting a sanitized error intended for final users.
    :rtype: Run
    """

    complain_on_error = kwargs.pop('complain_on_error', True)
    debug = kwargs.pop('debug', True)
    quiet = kwargs.pop('quiet', True)
    if kwargs:
        first_unknown_kwarg = sorted(kwargs)[0]
        raise ValueError('Invalid argument: {}'.format(first_unknown_kwarg))

    argv = ['alr']
    if debug:
        argv.append('-d')
    if quiet:
        argv.append('-q')
    argv.extend(args)
    p = Run(argv)
    if p.status != 0 and complain_on_error:
        print('The following command:')
        print('  {}'.format(' '.join(quote_arg(arg) for arg in argv)))
        print('Exitted with status code {}'.format(p.status))
        print('Output:')
        print(p.out)
        raise CalledProcessError('alr returned non-zero status code')
    return p


def fixtures_path(*args):
    """
    Return a path under the testsuite `fixtures` directory.
    """
    return os.path.join(TESTSUITE_ROOT, 'fixtures', *args)


def prepare_indexes(config_dir, working_dir, index_descriptions):
    """
    Populate alr's config directory with the provided indexes.

    :param str config_dir: Configuration directory for "alr".

    :param dict[str, dict] index_description: Mapping from index names to data
        used to prepare the corresponding indexes. This data dictionary accepts
        the following keys:

        * "dir" (str): Name of the directory that contains the index files. It
          is optional: if not provided, use the name of the index.

        * "in_fixtures" (bool): Whether the directory that contains the index
          files is to be found in the "fixtures" directory (by default). Look
          in the testcase's directory otherwise.

        * "priority" (int): Priority for this index. 1 by default.
    """
    indexes_dir = os.path.join(config_dir, 'indexes')
    mkdir(indexes_dir)

    for name, desc in index_descriptions.items():
        # Extract individual fields in `desc`
        def invalid_desc(reason):
            raise ValueError('invalid index description for {}: {}'
                             .format(name, reason))

        def check_type(what, type_name, value, typ):
            if not isinstance(value, typ):
                invalid_desc('{} must be a {}'.format(what, type_name))

        check_type('description', 'a dictionary', desc, dict)

        files_dir = desc.pop('dir', name)
        check_type('"dir"', 'a string', files_dir, str)

        in_fixtures = desc.pop('in_fixtures', True)
        check_type('"in_fixtures"', 'a boolean', in_fixtures, bool)

        priority = desc.pop('priority', 1)
        check_type('"priority"', 'an integer', priority, int)

        if desc:
            first_unknown_key = sorted(desc)[0]
            invalid_desc('unknown key {}'.format(repr(first_unknown_key)))

        # Compute the directory that contains index files
        files_dir = (fixtures_path(files_dir)
                     if in_fixtures else
                     files_dir)

        # Finally create the index description in the config directory
        index_dir = os.path.join(indexes_dir, name)
        mkdir(index_dir)
        with open(os.path.join(index_dir, 'index.toml'), 'w') as f:
            f.write("""
                name = "{}"
                priority = {}
                url = "{}"
            """.format(name, priority, files_dir))
