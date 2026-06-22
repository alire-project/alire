"""
Duplicate `pragma Alire_Test` key: the parser raises, the runner logs an error
and falls back to defaults.

NOTE: this behavior will likely change in a future PR to the selected failure
mode in settings.
"""

import os

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_match, assert_substring
from drivers.helpers import testing_write_test


init_local_crate(with_test=True)
# Drop the default failing test to avoid confusion with its output
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

testing_write_test(
    "dup_key",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "first");\n'
        'pragma Alire_Test (Name, "second");\n'
    ),
)

# quiet=False keeps the error line in the captured output.
p = run_alr("test", quiet=False)
assert_substring("duplicate Alire_Test pragma key", p.out)
# Display name falls back to the path-derived form, not "first" nor "second".
assert_match(r".*\[ PASS \] *\d+[smh]\d+ dup_key.*", p.out)


print("SUCCESS")
