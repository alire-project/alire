"""
Verify that `pragma Alire_Test (...)` clauses in test sources are honored
by the alr test runner, or that error conditions are properly handled.
"""

import json
import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import (
    assert_eq,
    assert_match,
    assert_not_substring,
    assert_substring,
)


def write_test(stem: str, body: str, prelude: str = "") -> None:
    """
    Write a test source file under tests/src/. `stem` is the suffix after
    `xxx_tests-`, e.g. "named" yields tests/src/xxx_tests-named.adb. `prelude`
    is inserted ahead of the unit declaration, where compilation pragmas
    must live. The generated test crate carries a `pragma Ignore_Pragma
    (Alire_Test)` configuration pragma, so `pragma Alire_Test` needs no local
    warning suppression to survive a -gnatwe build.
    """
    proc = f"Xxx_Tests.{stem.title()}"
    path = f"./tests/src/xxx_tests-{stem}.adb"
    with open(path, "w") as f:
        f.write(prelude)
        f.write(f"procedure {proc} is\n")
        f.write(f"begin\n   {body}\nend {proc};\n")


# Later used to ensure all expected tests are there
def find_test(tests, name):
    for t in tests:
        if t["name"] == name:
            return t
    raise AssertionError(f"no test named {name!r} in {[t['name'] for t in tests]}")


# Return tests results as JSON
def parse_json_result(p):
    """Return the parsed JSON object from p.out.

    Trace.Error/Warning go to stderr, which e3 merges with stdout, so p.out may
    contain diagnostic lines before the JSON blob. The JSON itself may be
    pretty-printed across multiple lines, so we find the first line that starts
    with '{' and parse from there to end of output.
    """
    lines = p.out.splitlines()
    for i, line in enumerate(lines):
        if line.lstrip().startswith("{"):
            return json.loads("\n".join(lines[i:]))
    raise AssertionError(
        f"no JSON object found in output:\n{p.out}"
    )


init_local_crate("xxx", with_test=True)

# Drop the default failing test that init seeds.
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

# Supported keys: Name, Should_Fail, Timeout in a single prelude.
# Body raises, but Should_Fail=True means this counts as a pass.

write_test(
    "triple",
    "raise Program_Error;",
    prelude=(
        'pragma Alire_Test (Name,        "pragma_triple");\n'
        "pragma Alire_Test (Should_Fail, True);\n"
        "pragma Alire_Test (Timeout,     11);\n"
    ),
)

# Compact: no extra whitespace.

write_test(
    "compact",
    "null;",
    prelude='pragma Alire_Test(Name,"pragma_compact");\n',
)

# Extra spaces around punctuation.

write_test(
    "spaced",
    "null;",
    prelude='pragma   Alire_Test  (  Name  ,  "pragma_spaced"  )  ;\n',
)

# Tab-separated tokens.

write_test(
    "tabbed",
    "null;",
    prelude='pragma Alire_Test\t(Name\t,"pragma_tabbed");\n',
)

# Pragma body split across lines.

write_test(
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

write_test(
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

write_test(
    "other_pragma",
    "null;",
    prelude=(
        "pragma Ignore_Pragma (Other_Pragma);\n"
        'pragma Alire_Test (Name, "pragma_two_names");\n'
        'pragma Other_Pragma (Name, "ignored");\n'
    ),
)

# Should_Fail=False (explicit) with a passing body.

write_test(
    "explicit_no_fail",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "pragma_no_fail");\n'
        "pragma Alire_Test (Should_Fail, False);\n"
    ),
)

# Named form: `pragma Alire_Test (Key => Value)` is accepted as
# equivalent to the positional `(Key, Value)` form.

write_test(
    "named",
    "null;",
    prelude='pragma Alire_Test (Name => "pragma_named");\n',
)

# No Alire_Test pragma at all (overkill but...)

write_test("plain", "null;")

# Case-insensitive pragma name and keys

write_test(
    "mixed_case",
    "raise Program_Error;",
    prelude=(
        'pragma alire_test (nAmE, "Pragma_Mixed_Case");\n'
        "pragma ALIRE_TEST (should_FAIL, TRUE);\n"
    ),
)

# Run all tests and capture JSON output for verification

p = run_alr("--format=json", "test")
data = parse_json_result(p)

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

assert_eq("pass", find_test(tests, "pragma_triple")["status"])

# Case-insensitive Should_Fail=True does the same; the case-mangled Name
# is applied as the display name.

assert_eq("pass", find_test(tests, "Pragma_Mixed_Case")["status"])

# A source with no Alire_Test pragma runs normally under its derived name.

assert_eq("pass", find_test(tests, "plain")["status"])

# Duplicate Alire_Test key: parser raises, runner logs an error and falls back
# to defaults. The run still completes, we just verify the error and the
# fallback name.

os.chdir("..")
shutil.rmtree("xxx")

init_local_crate("xxx", with_test=True)
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")
write_test(
    "dup_key",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "first");\n'
        'pragma Alire_Test (Name, "second");\n'
    ),
)

# The runner logs an error but still runs the test with defaults, so exit
# status stays zero. quiet=False keeps the error line in the captured output.

p = run_alr("test", quiet=False)
assert_substring("duplicate Alire_Test pragma key", p.out)
# Display name falls back to the path-derived form, not "first" nor "second".
assert_match(r".*\[ PASS \] *\d+[smh]\d+ dup_key.*", p.out)

# Malformed Alire_Test pragma: strict mode causes Invalid_Pragma_Syntax,
# which the runner translates to a pre-run failure. The test is marked
# FAIL without being spawned. We exercise both the positional form with
# an unsupported value expression, and the named form (`=>`) with the
# same problem.
for stem, prelude in (
    ("bad_syntax_pos",   'pragma Alire_Test (Timeout,   1.0 * 60.0);\n'),
    ("bad_syntax_named", 'pragma Alire_Test (Timeout => 1.0 * 60.0);\n'),
):
    os.chdir("..")
    shutil.rmtree("xxx")

    init_local_crate("xxx", with_test=True)
    os.remove("./tests/src/xxx_tests-assertions_enabled.adb")
    write_test(stem, "null;", prelude=prelude)

    p = run_alr("test", quiet=False, complain_on_error=False)
    assert_substring("failed to parse strict pragma", p.out)
    assert_match(rf".*\[ FAIL \] *\d+[smh]\d+ {stem}.*", p.out)

# Unrecognized Alire_Test key: the response action is defined by
# tests.on_unknown_parameter, which can be 'ignore', 'fail' (default) or
# 'skip'. We exercise all three values against the same source tree.

os.chdir("..")
shutil.rmtree("xxx")

init_local_crate("xxx", with_test=True)
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")
write_test(
    "bogus_key",
    "null;",
    prelude='pragma Alire_Test (Bogus, "anything");\n',
)

# Default ('fail'): the test is reported as a failure without running, and
# the runner exits non-zero. The diagnostic names the offending key.

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

# Structured output exposes the skip in both the per-test entry and the
# summary.

p = run_alr("--format=json", "test")
data = parse_json_result(p)
assert_eq(1, data["summary"]["total"])
assert_eq(0, data["summary"]["failures"])
assert_eq(1, data["summary"]["skipped"])
assert_eq("skip", find_test(data["tests"], "bogus_key")["status"])
assert_substring(
    "unknown Alire_Test pragma key: bogus",
    find_test(data["tests"], "bogus_key")["reason"],
)


print("SUCCESS")
