"""
Unrecognized `pragma Alire_Test` key: the response action is defined by
tests.on_unknown_parameter, which can be 'ignore', 'fail' (default) or
'skip'.
"""

import os

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import (
    assert_eq,
    assert_match,
    assert_not_substring,
    assert_substring,
)
from drivers.helpers import (
    testing_find_test,
    testing_parse_json_result,
    testing_write_test,
)


init_local_crate(with_test=True)
# Drop the default failing test to avoid confusion with its output
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

testing_write_test(
    "bogus_key",
    "null;",
    prelude='pragma Alire_Test (Bogus, "anything");\n',
)

# Default ('fail'): the test is reported as a failure without running, and the
# runner exits non-zero. The diagnostic names the offending key.

p = run_alr("test", quiet=False, complain_on_error=False)
assert_substring("unknown Alire_Test pragma key", p.out)
# Keys are normalized to lower case by the parser, so the diagnostic echoes
# 'bogus', not the source spelling 'Bogus'.
assert_substring("'bogus'", p.out)
assert_match(r".*\[ FAIL \] *\d+[smh]\d+ bogus_key.*", p.out)

# 'ignore': the test runs to completion as if no unknown key were there.

run_alr(
    "settings", "--global", "--set",
    "tests.on_unknown_parameter", "ignore",
)
p = run_alr("test", quiet=False)
assert_not_substring("unknown Alire_Test pragma key", p.out)
assert_match(r".*\[ PASS \] *\d+[smh]\d+ bogus_key.*", p.out)

# 'skip': the test is reported as SKIP with the reason, and does not count
# towards failures.

run_alr(
    "settings", "--global", "--set",
    "tests.on_unknown_parameter", "skip",
)
p = run_alr("test", quiet=False)
assert_substring("unknown Alire_Test pragma key", p.out)
assert_match(
    r".*\[ SKIP \].*bogus_key \(unknown Alire_Test pragma key: bogus\).*",
    p.out,
)
assert_not_substring("[ PASS ]", p.out)
assert_not_substring("[ FAIL ]", p.out)

# Structured output exposes the skip in both the per-test entry and the summary

p = run_alr("--format=json", "test")
data = testing_parse_json_result(p)
assert_eq(1, data["summary"]["total"])
assert_eq(0, data["summary"]["failures"])
assert_eq(1, data["summary"]["skipped"])
assert_eq("skip", testing_find_test(data["tests"], "bogus_key")["status"])
assert_substring(
    "unknown Alire_Test pragma key: bogus",
    testing_find_test(data["tests"], "bogus_key")["reason"],
)


print("SUCCESS")
