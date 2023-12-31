"""
Test that Alire properly reports an invalid index URL.
"""

import os
import re

from e3.fs import rm

from drivers.alr import prepare_indexes, run_alr
from drivers.asserts import assert_match

def comparator(param):
    """If the test value (in d) is 'no-such-directory', the value tested for
    is './no-such-directory'; if not, it's left as it is."""
    if param == 'no-such-directory': return './no-such-directory'
    return param

for d in ('no-such-directory',
          'file://no-such-directory', ):
    rm('alr-config', recursive=True)
    prepare_indexes('alr-config', '.',
                    {'bad_index': {'dir': d, 'in_fixtures': False}})
    p = run_alr("search", "--crates", complain_on_error=False, debug=False)

    path_excerpt = os.path.join('alr-config', 'indexes', 'bad_index',
                                'index.toml')
    assert_match('ERROR: Cannot load metadata from .*{}:'
                 ' Not a readable directory: {}'
                 '\n'
                 .format(re.escape(path_excerpt), comparator(d)),
                 p.out)

print('SUCCESS')
