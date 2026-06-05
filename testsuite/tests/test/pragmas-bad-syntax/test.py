"""
Malformed `pragma Alire_Test`: the runner must reject it rather than silently
accept it.
"""

import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_substring, assert_match
from drivers.helpers import testing_write_test


def fresh_crate():
    # Each case needs an independent crate; tear down the previous one (we are
    # left inside it by init_local_crate) before building the next.
    if os.path.basename(os.getcwd()) == "xxx":
        os.chdir("..")
        shutil.rmtree("xxx")
    init_local_crate(with_test=True)
    # Drop the default failing test to avoid confusion with its output
    os.remove("./tests/src/xxx_tests-assertions_enabled.adb")


# A value the strict parser cannot handle yields "failed to parse strict
# pragma"; a recognized key given the wrong kind of value yields "unexpected
# value type". Both are rejected pre-run as a FAIL.

for stem, prelude, diagnostic in (
    ("bad_syntax_pos",     'pragma Alire_Test (Timeout,   1.0 * 60.0);\n',
     "failed to parse strict pragma"),
    ("bad_syntax_named",   'pragma Alire_Test (Timeout => 1.0 * 60.0);\n',
     "failed to parse strict pragma"),
    ("bad_syntax_missing", 'pragma Alire_Test (Timeout);\n',
     "Alire_Test pragma key 'timeout' has an unexpected value type"),
):
    fresh_crate()
    testing_write_test(stem, "null;", prelude=prelude)

    p = run_alr("test", quiet=False, complain_on_error=False)
    assert_substring(diagnostic, p.out)
    assert_match(rf".*\[ FAIL \] *\d+[smh]\d+ {stem}.*", p.out)


print("SUCCESS")
