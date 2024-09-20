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


# Delete old configuration and indexes, but disable msys2 installation or
# installation will be reattempted.
rm('alr-config', recursive=True)
run_alr("settings", "--global", "--set", "msys2.do_not_install", "true")


# Directly configure the non-existent index in Alire's config directory
prepare_indexes(
    'alr-config', '.', {'bad_index': {'dir': INDEX_DIR, 'in_fixtures': False}}
)
# Verify that `alr search` gives a suitable error
p = run_alr("search", "--crates", complain_on_error=False, debug=False)
rel_path = os.path.join('alr-config', 'indexes', 'bad_index', 'index.toml')
separator = re.escape(os.path.sep)
err_msg = (
    f'.*ERROR: Cannot load metadata from .*{re.escape(rel_path)}: '
    f'Not a readable directory: .{separator}{INDEX_DIR}\n'
)
assert_match(err_msg, p.out)

# Rewrite the url field of the config 'index.toml' file to use a 'file://' URL
replace_in_file(rel_path, "url = '", "url = 'file://")
# Verify this yields the same result
p = run_alr("search", "--crates", complain_on_error=False, debug=False)
assert_match(err_msg, p.out)

# Repeat both cases, but using the `alr index --add` UI
p = run_alr(
    "index", "--add", "no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(err_msg, p.out)
p = run_alr(
    "index", "--add", "file:no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(err_msg, p.out)
p = run_alr(
    "index", "--add", "file://no-such-directory", "--name", "bad_index",
    complain_on_error=False
)
assert_match(err_msg, p.out)


print('SUCCESS')
