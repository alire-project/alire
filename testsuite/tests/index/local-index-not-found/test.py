"""
Test that Alire properly reports an invalid index URL.
"""

import os
import os.path
import re

from e3.fs import rm

from drivers.alr import prepare_indexes, run_alr
from drivers.asserts import assert_match
from drivers.helpers import replace_in_file


INDEX_DIR = "no-such-directory"
REL_CONF_PATH = os.path.join('alr-config', 'indexes', 'bad_index', 'index.toml')
ERR_MSG = (
    f'.*ERROR: Cannot load metadata from .*{re.escape(REL_CONF_PATH)}: '
    f'Not a readable directory: .{re.escape(os.path.sep)}{INDEX_DIR}\n'
)


# Directly configure the non-existent index in Alire's config directory
prepare_indexes(
    'alr-config', '.', {'bad_index': {'dir': INDEX_DIR, 'in_fixtures': False}}
)
# Verify that `alr search` gives a suitable error
p = run_alr("search", "--crates", complain_on_error=False, debug=False)
assert_match(ERR_MSG, p.out)

# Rewrite the url field of the config 'index.toml' file to use a 'file://' URL
replace_in_file(REL_CONF_PATH, "url = '", "url = 'file://")
# Verify this yields the same result
p = run_alr("search", "--crates", complain_on_error=False, debug=False)
assert_match(ERR_MSG, p.out)

# Repeat both cases, but using the `alr index --add` UI
p = run_alr(
    "index", "--add", "no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(ERR_MSG, p.out)
p = run_alr(
    "index", "--add", "file:no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(ERR_MSG, p.out)
p = run_alr(
    "index", "--add", "file://no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(ERR_MSG, p.out)


print('SUCCESS')
