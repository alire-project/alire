"""
Verify that valid `pragma Alire_Test (...)` clauses in test sources are
honored by the alr test runner.
"""

import os

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq
from drivers.helpers import (
    testing_find_test,
    testing_parse_json_result,
    testing_write_test,
)


init_local_crate("xxx", with_test=True)

# Drop the default failing test that init seeds.
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

# Supported keys: Name, Should_Fail, Timeout in a single prelude.
# Body raises, but Should_Fail=True means this counts as a pass.

testing_write_test(
    "triple",
    "raise Program_Error;",
    prelude=(
        'pragma Alire_Test (Name,        "pragma_triple");\n'
        "pragma Alire_Test (Should_Fail, True);\n"
        "pragma Alire_Test (Timeout,     11);\n"
    ),
)

# Compact: no extra whitespace.

testing_write_test(
    "compact",
    "null;",
    prelude='pragma Alire_Test(Name,"pragma_compact");\n',
)

# Extra spaces around punctuation.

testing_write_test(
    "spaced",
    "null;",
    prelude='pragma   Alire_Test  (  Name  ,  "pragma_spaced"  )  ;\n',
)

# Tab-separated tokens.

testing_write_test(
    "tabbed",
    "null;",
    prelude='pragma Alire_Test\t(Name\t,"pragma_tabbed");\n',
)

# Pragma body split across lines.

testing_write_test(
    "multiline",
    "null;",
    prelude=(
        "pragma Alire_Test\n"
        "  ( Name\n"
        '  , "pragma_multiline"\n'
        "  );\n"
    ),
)

# Comments between tokens.

testing_write_test(
    "annotated",
    "null;",
    prelude=(
        "pragma Alire_Test -- the pragma name\n"
        "  ( Name         -- the key\n"
        '  , "pragma_annotated"\n'
        "  );\n"
    ),
)

# Two distinct pragma names: only Alire_Test should be applied. The foreign
# pragma is locally silenced with `pragma Ignore_Pragma` so it does not trip
# the -gnatwe switch (the crate-wide config pragma only ignores Alire_Test).

testing_write_test(
    "other_pragma",
    "null;",
    prelude=(
        "pragma Ignore_Pragma (Other_Pragma);\n"
        'pragma Alire_Test (Name, "pragma_two_names");\n'
        'pragma Other_Pragma (Name, "ignored");\n'
    ),
)

# Should_Fail=False (explicit) with a passing body.

testing_write_test(
    "explicit_no_fail",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "pragma_no_fail");\n'
        "pragma Alire_Test (Should_Fail, False);\n"
    ),
)

# Named form: `pragma Alire_Test (Key => Value)` is accepted as
# equivalent to the positional `(Key, Value)` form.

testing_write_test(
    "named",
    "null;",
    prelude='pragma Alire_Test (Name => "pragma_named");\n',
)

# No Alire_Test pragma at all (overkill but...)

testing_write_test("plain", "null;")

# Case-insensitive pragma name and keys

testing_write_test(
    "mixed_case",
    "raise Program_Error;",
    prelude=(
        'pragma alire_test (nAmE, "Pragma_Mixed_Case");\n'
        "pragma ALIRE_TEST (should_FAIL, TRUE);\n"
    ),
)

# Run all tests and capture JSON output for verification

p = run_alr("--format=json", "test")
data = testing_parse_json_result(p)

# Verify no failures in testing

assert_eq(11, data["summary"]["total"])
assert_eq(0, data["summary"]["failures"])

# Verify names of tests

tests = data["tests"]
names = sorted(t["name"] for t in tests)
assert_eq(
    [
        "Pragma_Mixed_Case",
        "plain",
        "pragma_annotated",
        "pragma_compact",
        "pragma_multiline",
        "pragma_named",
        "pragma_no_fail",
        "pragma_spaced",
        "pragma_tabbed",
        "pragma_triple",
        "pragma_two_names",
    ],
    names,
)

# Should_Fail=True must turn a raising test into a pass.

assert_eq("pass", testing_find_test(tests, "pragma_triple")["status"])

# Case-insensitive Should_Fail=True does the same; the case-mangled Name
# is applied as the display name.

assert_eq("pass", testing_find_test(tests, "Pragma_Mixed_Case")["status"])

# A source with no Alire_Test pragma runs normally under its derived name.

assert_eq("pass", testing_find_test(tests, "plain")["status"])


print("SUCCESS")
