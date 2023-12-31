"""
Test that Alire properly reports an invalid index URL.
"""

import os
import re

from e3.fs import rm

from drivers.alr import prepare_indexes, run_alr
from drivers.asserts import assert_match


for d in ('./no-such-directory',
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
                 .format(re.escape(path_excerpt), d),
                 p.out)

print('SUCCESS')
