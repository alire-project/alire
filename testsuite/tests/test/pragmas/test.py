"""
Verify that `pragma Alire_Test (...)` clauses in test sources are honored
by the alr test runner. Mirrors the pragma-input cases from
deps/lml/test/src/test_pragmas.adb, but exercises the full pipeline
through `alr test` instead of the parser in isolation.
"""

import json
import os
import shutil

from drivers.alr import init_local_crate, run_alr
from drivers.asserts import assert_eq, assert_match, assert_substring


def write_test(stem: str, body: str, prelude: str = "") -> None:
    """
    Write a test source file under tests/src/. `stem` is the suffix after
    `xxx_tests-`, e.g. "named" yields tests/src/xxx_tests-named.adb. `prelude`
    is inserted ahead of the unit declaration, where compilation pragmas
    must live. The unrecognized-pragma warning is suppressed locally so
    `pragma Alire_Test` does not break -gnatwe builds.
    """
    proc = f"Xxx_Tests.{stem.title()}"
    path = f"./tests/src/xxx_tests-{stem}.adb"
    with open(path, "w") as f:
        f.write('pragma Warnings (Off, "unrecognized pragma*");\n')
        f.write(prelude)
        f.write('pragma Warnings (On, "unrecognized pragma*");\n')
        f.write(f"procedure {proc} is\n")
        f.write(f"begin\n   {body}\nend {proc};\n")


def find_test(tests, name):
    for t in tests:
        if t["name"] == name:
            return t
    raise AssertionError(f"no test named {name!r} in {[t['name'] for t in tests]}")


init_local_crate("xxx", with_test=True)

# Drop the default failing test that init seeds.
os.remove("./tests/src/xxx_tests-assertions_enabled.adb")

# --- supported triple: Name, Should_Fail, Timeout in a single prelude ---
# Body raises, but Should_Fail=True means this counts as a pass; the
# Timeout value must be accepted without breaking the runner.
write_test(
    "triple",
    "raise Program_Error;",
    prelude=(
        'pragma Alire_Test (Name,        "pragma_triple");\n'
        "pragma Alire_Test (Should_Fail, True);\n"
        "pragma Alire_Test (Timeout,     11);\n"
    ),
)

# --- compact: no extra whitespace ---
write_test(
    "compact",
    "null;",
    prelude='pragma Alire_Test(Name,"pragma_compact");\n',
)

# --- extra spaces around punctuation ---
write_test(
    "spaced",
    "null;",
    prelude='pragma   Alire_Test  (  Name  ,  "pragma_spaced"  )  ;\n',
)

# --- tab-separated tokens ---
write_test(
    "tabbed",
    "null;",
    prelude='pragma Alire_Test\t(Name\t,"pragma_tabbed");\n',
)

# --- pragma body split across lines ---
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

# --- comments between tokens ---
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

# --- two distinct pragma names: only Alire_Test should be applied ---
write_test(
    "other_pragma",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "pragma_two_names");\n'
        'pragma Other_Pragma (Tag, "ignored");\n'
    ),
)

# --- Should_Fail=False (explicit) with a passing body ---
write_test(
    "explicit_no_fail",
    "null;",
    prelude=(
        'pragma Alire_Test (Name, "pragma_no_fail");\n'
        "pragma Alire_Test (Should_Fail, False);\n"
    ),
)

p = run_alr("--format=json", "test")
data = json.loads(p.out)

assert_eq(8, data["summary"]["total"])
assert_eq(0, data["summary"]["failures"])

tests = data["tests"]
names = sorted(t["name"] for t in tests)
assert_eq(
    [
        "pragma_annotated",
        "pragma_compact",
        "pragma_multiline",
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

# --- duplicate Alire_Test key: parser raises, runner logs an error and
# falls back to defaults (test name derived from the file). The run still
# completes; we just want to see the error and the fallback name. ---
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
# Display name falls back to the path-derived form, not "first" / "second".
assert_match(r".*\[ PASS \] *\d+[smh]\d+ dup_key.*", p.out)

print("SUCCESS")
